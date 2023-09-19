module Contract 
  ( module Contract.Script
  , Address
  , Value
  , ContractResult
  , TransactionId
  , ownWalletAddress
  , deposit
  , withdraw
  )
  where

import Contract.Prelude
  ( ($)
  , (<>)
  , (<$>)
  , Unit
  , Maybe(Nothing)
  , pure
  , bind
  , discard
  , liftEither
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
import Contract.MerkleTree ( Proof )
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
  }
type ContractResult =
  { txId :: TransactionId
  , txFinalFee :: Value
  }

newtype Redeemer = Redeemer
    { proof :: Proof
    , element :: String
    }

instance CPD.ToData Redeemer where
  toData (Redeemer { proof, element }) = CPD.Constr CNBN.zero
    [ CPD.toData proof
    , CPD.toData element
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
withdraw p = do
  validator <- liftEither validator
  let scriptAddress = CA.scriptHashAddress
        (CS.validatorHash validator)
        Nothing
  utxos <- CU.utxosAt scriptAddress
  utxo <- CM.liftContractM "could not find utxo at script address" $
    DA.head $ CT.lookupTxHash p.depositTxId utxos
  let
      txInput = view CT._input utxo
      constraints :: CTC.TxConstraints Unit Unit
      constraints =    CTC.mustSpendScriptOutput txInput CPD.unitRedeemer

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
