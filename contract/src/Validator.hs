{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Validator (validator) where

import PlutusTx.Prelude
  ( Bool (True, False)
  , BuiltinByteString
  , Integer
  , (==)
  , (&&)
  )
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.MerkleTree
  ( Hash
  , Proof
  , member
  )
import Plutus.V2.Ledger.Api (Validator, ScriptContext, mkValidatorScript)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

type Datum = Hash
data Redeemer = Redeemer
  { element :: BuiltinByteString
  , proof :: Proof
  }
unstableMakeIsData ''Redeemer

validator_ :: Datum -> Redeemer -> ScriptContext -> Bool
validator_ root (Redeemer { proof, element }) _ =
  member element root proof 

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator_
