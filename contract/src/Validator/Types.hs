{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
module Validator.Types where

import PlutusTx.Prelude
  ( BuiltinByteString
  )
import PlutusTx.MerkleTree
  ( Hash
  , Proof
  )

type Element = BuiltinByteString

type Datum = Hash

data Redeemer = Redeemer
  { element :: Element
  , proof :: Proof
  }

