module Test.Main where

import Contract.Prelude
  ( ($)
  , (=<<)
  , (+)
  , (-)
  , (<>)
  , (<$>)
  , LogLevel (Trace)
  , Unit
  , flip
  , bind
  , pure
  , void
  , discard
  , show
  , liftEffect
  )

import Control.Monad.Trans.Class (lift)
import Contract
  ( Address
  , ContractResult
  , Withdraw
  , validator
  , deposit
  , withdraw
  )
import Contract.Layer2
  ( TxData
  , Tx
  , tree
  , tx1
  , tx2
  , tx3
  )
import Contract.Address as CA
import Contract.Config (emptyHooks)
import Contract.Monad as CM
import Contract.Test
  ( ContractTest
  , InitialUTxOs
  , withWallets
  , withKeyWallet
  )
import Contract.Test.Mote
  ( TestPlanM
  , interpretWithConfig
  )
import Contract.Test.Plutip
  ( PlutipConfig
  , testPlutipContracts
  )
import Contract.Test.Utils as CTU
import Contract.Test.Assert as CTA
import Contract.Scripts as CS
import Contract.Crypto (hash, combineHash)
import Contract.MerkleTree
  ( rootHash
  , mkProof
  )
import Contract.Log as CL
import Data.Array as DA
import Data.BigInt as DBI
import Data.Maybe (Maybe (Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds (Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt as DU
import Effect (Effect)
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (test)
import Test.Spec.Runner (defaultConfig)

type Labeled = CTA.Labeled
type Contract = CM.Contract
type Params = ( script :: Labeled Address )
type DepositParams
  = { depositor :: Labeled Address
    | Params  
    }
type WithdrawParams
  = { beneficiary :: Labeled Address
    | Params
    }

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: DU.fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: DU.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: DU.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05 }
  }

withPaymentPubKeyHash :: forall a. Labeled Address -> (CA.PaymentPubKeyHash -> Contract a) -> Contract a
withPaymentPubKeyHash addr run = do
  pkh <- CM.liftContractM "failed to get address pub key hash" $
    CA.toPubKeyHash (CTA.unlabel addr)
  run $ CA.PaymentPubKeyHash pkh

getOwnWalletLabeledAddress :: String -> Contract (Labeled Address)
getOwnWalletLabeledAddress s = do
       addr <- CM.liftedM ("Failed to get " <> s <> " address") $
         DA.head <$> CA.getWalletAddresses
       pure $ CTA.label addr s

getScriptAddress :: Contract (Labeled Address)
getScriptAddress = do
        validator' <- CM.liftContractE' "Failed to parse validator" $
         CS.validatorHash <$> validator
        nId <- CA.getNetworkId
        valAddr <- CM.liftContractM "Failed to get validator address" $
          CA.validatorHashEnterpriseAddress nId validator'
        pure $ CTA.label valAddr "script"

suite :: TestPlanM ContractTest Unit
suite = do
  test "depositor locks ADA and a root hash in the contract" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ DBI.fromInt 5_000_000
        , DBI.fromInt 2_000_000_000
        ]
      checks :: DepositParams -> Array (CTA.ContractCheck ContractResult)
      checks { depositor, script } = let amount = DBI.fromInt 10_000_000 in
        [ CTA.checkLossAtAddress depositor
          \r -> do
             { txFinalFee } <- CM.liftContractM "contract did not provide value" r
             pure $ amount + txFinalFee
        , CTA.checkGainAtAddress' script amount
        ]
    withWallets distribution \kw -> withKeyWallet kw do
       depositor <- getOwnWalletLabeledAddress "depositor"
       script <- getScriptAddress
       tree_ <- liftEffect tree
       let value = DBI.fromInt 10_000_000
           root = rootHash tree_
       void $ CTA.runChecks
        ( checks { depositor, script } )
        ( lift $ deposit { root, value } )
  test "beneficiary proves element is in the markle tree and unlocks all ADA in the contract" do
    let
      distribution =
           [ DBI.fromInt 20_000_000
           , DBI.fromInt 5_000_000
           ]
        /\ [ DBI.fromInt 1_000_000
           , DBI.fromInt 5_000_000
           ]
      checks :: WithdrawParams -> Array (CTA.ContractCheck ContractResult)
      checks { beneficiary, script } =
        let amount = DBI.fromInt 10_000_000
         in
        [ CTA.checkGainAtAddress beneficiary
          \r -> do
             { txFinalFee } <- CM.liftContractM
               "contract did not provide any value" 
               r
             pure $ amount - txFinalFee
        , CTA.checkLossAtAddress' script amount
        ]
    withWallets distribution \(w1 /\ w2) -> do
       beneficiary <- withKeyWallet w2 $ getOwnWalletLabeledAddress "beneficiary"
       tree_ <- liftEffect tree
       let value = DBI.fromInt 10_000_000
           root = rootHash tree_
       { txId: depositTxId } <- withKeyWallet w1 $ deposit { root, value }
       withKeyWallet w2 do
          script <- getScriptAddress
          let element = tx2
              tx2_ = hash tx2
              tx3_ = hash tx3
          proof <- CM.liftContractM
            "could not produce the proof"
            (mkProof element tree_)
          CL.logTrace' $ "Tx2: " <> show tx2_
          CL.logTrace' $ "Tx3: " <> show tx3_
          c <- liftEffect (tx2_ `combineHash` tx3_)
          CL.logTrace' $ "Tx2<>Tx3: " <> show c
          CL.logTrace' $ "Root: " <> show root
          CL.logTrace' $ "Tree: " <> show tree_
          CL.logTrace' $ "Proof: " <> show proof
          CL.logTrace' $ "Element: " <> show element
          void $ CTA.runChecks
            ( checks
                { beneficiary
                , script
                }
            )
            ( lift $ withdraw
                { element
                , root
                , proof
                , depositTxId
                }
            )

main :: Effect Unit
main = CTU.interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (CTU.exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

