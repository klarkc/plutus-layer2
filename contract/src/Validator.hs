{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Validator (validator) where

import PlutusTx.Prelude
  ( Bool (True, False)
  , BuiltinByteString
  )
import PlutusTx (compile, unstableMakeIsData)
import Plutus.V2.Ledger.Api (Validator, ScriptContext, mkValidatorScript)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))
import Plutus.MerkleTree
  ( Hash
  , Proof
  , member
  )

data Redeemer = Redeemer
  { proof :: Proof
  , element :: BuiltinByteString
  }
unstableMakeIsData ''Redeemer

validator_ :: Hash -> Redeemer -> ScriptContext -> Bool
validator_ root (Redeemer { proof, element }) _ | member element root proof = True 
validator_ _ _ _ = False 

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator_
