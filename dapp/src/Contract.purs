module Contract 
  ( module Contract.Types
  , module Contract.Script
  , ownWalletAddress
  , deposit
  , withdraw
  , tx1
  , tx2
  , tx3
  , tree
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
import Contract.Types
  ( ContractResult
  , TransactionId
  , TxData
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
  ( MerkleTree
  , Proof
  , fromFoldable
  , rootHash
  )
import Data.Array as DA
import Data.Lens (view)
import Contract.Script (validator)
import Contract.Types as CT

newtype Redeemer = Redeemer
    { proof :: Proof
    , element :: String
    }

instance CPD.ToData Redeemer where
  toData (Redeemer { proof, element }) = CPD.Constr CNBN.zero
    [ CPD.toData proof
    , CPD.toData element
    ]

tx1 :: TxData
tx1 = "tx1Data"

tx2 :: TxData
tx2 = "tx2Data"

tx3 :: TxData
tx3 = "tx3Data"

tree :: MerkleTree TxData
tree = fromFoldable [ tx1, tx2, tx3 ]

ownWalletAddress :: String -> CM.Contract CA.Address
ownWalletAddress s = CM.liftedM ("Failed to get " <> s <> " address") $
  DA.head <$> CA.getWalletAddresses

deposit :: CT.Deposit -> CM.Contract CT.ContractResult
deposit dp = do
  validator <- liftEither validator
  let
      value = CV.lovelaceValueOf dp.value
      vhash = CS.validatorHash validator
      datum = wrap $ CPD.toData $ rootHash tree
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

withdraw :: CT.Withdraw -> CM.Contract CT.ContractResult
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
