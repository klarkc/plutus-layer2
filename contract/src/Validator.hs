{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Validator (validator) where

import PlutusTx.Prelude
  ( Bool
  )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.MerkleTree (member)
import Plutus.V2.Ledger.Api
  ( Validator
  , ScriptContext
  , mkValidatorScript
  )
import Plutus.Script.Utils.Typed
  ( IsScriptContext (mkUntypedValidator)
  )
import Validator.Types
  ( Datum
  , Redeemer
  , element
  , proof
  )
import Validator.Trace
  ( trace
  )

-- FIXME move this function to Validator.Types to avoid orphan instances
unstableMakeIsData ''Redeemer

validator_ :: Datum -> Redeemer -> ScriptContext -> Bool
validator_ root r _ = member (element r) root (proof r)

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator (trace validator_)
