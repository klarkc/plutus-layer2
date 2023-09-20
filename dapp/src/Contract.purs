module Contract 
  ( module Contract.Script
  , Address
  , Value
  , ContractResult
  , TransactionId
  , Withdraw
  , ownWalletAddress
  , deposit
  , withdraw
  )
  where

import Contract.Prelude
  ( ($)
  , (<>)
  , (<$>)
  , (>>=)
  , Unit
  , Maybe(Nothing)
  , pure
  , bind
  , discard
  , liftEither
  , liftEffect
  , wrap
  )
import Contract.Address as CA
import Contract.Monad as CM
import Contract.ScriptLookups as CSL
import Contract.Scripts as CS
import Contract.Transaction as CT
import Contract.TxConstraints as CTC
import Contract.Value as CV
import Contract.Utxos as CU
import Contract.PlutusData as CPD
import Contract.Numeric.BigNum as CNBN
import Contract.MerkleTree
  ( Proof
  , member
  )
import Data.Array as DA
import Data.Lens (view)
import Contract.Script (validator)
import Contract.Layer2
  ( Tx
  , TxData
  )
import Data.BigInt as DBI

type Value = DBI.BigInt
type TransactionId = CT.TransactionHash
type Address = CA.Address
type Deposit = 
  { value :: Value
  , root :: Tx
  }
type Withdraw =
  { depositTxId :: TransactionId
  , element :: TxData
  , proof :: Proof
  , root :: Tx
  }
type ContractResult =
  { txId :: TransactionId
  , txFinalFee :: Value
  }

data ContractReedemer = ContractRedeemer
  { element :: TxData
  , proof :: Proof
  }
instance CPD.ToData ContractReedemer where
  toData (ContractRedeemer { element, proof }) = CPD.Constr CNBN.zero
    [ CPD.toData element
    , CPD.toData proof
    ]

ownWalletAddress :: String -> CM.Contract CA.Address
ownWalletAddress s = CM.liftedM ("Failed to get " <> s <> " address") $
  DA.head <$> CA.getWalletAddresses

deposit :: Deposit -> CM.Contract ContractResult
deposit dp = do
  validator <- liftEither validator
  let
      value = CV.lovelaceValueOf dp.value
      vhash = CS.validatorHash validator
      datum = wrap $ CPD.toData dp.root
      constraints :: CTC.TxConstraints Unit Unit
      constraints = CTC.mustPayToScript vhash datum CTC.DatumWitness value
      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups = CSL.validator validator
  ubTx <- CM.liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- CM.liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }

withdraw :: Withdraw -> CM.Contract ContractResult
withdraw p = liftEffect (member p.element p.root p.proof) >>= case _ of
  false -> CM.throwContractError "could not prove element is in the tree"
  true -> do
    validator <- liftEither validator
    let scriptAddress = CA.scriptHashAddress
          (CS.validatorHash validator)
          Nothing
        redeemer = wrap $ CPD.toData $ ContractRedeemer
          { element: p.element
          , proof: p.proof
          }
    utxos <- CU.utxosAt scriptAddress
    utxo <- CM.liftContractM "could not find utxo at script address" $
      DA.head $ CT.lookupTxHash p.depositTxId utxos
    let
        txInput = view CT._input utxo
        constraints :: CTC.TxConstraints Unit Unit
        constraints = CTC.mustSpendScriptOutput txInput redeemer

        lookups :: CSL.ScriptLookups CPD.PlutusData
        lookups =    CSL.validator validator
                  <> CSL.unspentOutputs utxos
    ubTx <- CM.liftedE $ CSL.mkUnbalancedTx lookups constraints
    bsTx <- CM.liftedE $ CT.balanceTx ubTx
    tx <- CT.signTransaction bsTx
    txId <- CT.submit tx
    CT.awaitTxConfirmed txId
    pure { txId
         , txFinalFee: CT.getTxFinalFee tx
         }
