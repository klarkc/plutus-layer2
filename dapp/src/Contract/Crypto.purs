module Contract.Crypto
  ( module Exports
  , Hash(..)
  , hash
  , combineHash
  ) where

import Crypto.Simple
  ( class Hashable
  ) as Exports

import Prelude
  ( class Eq
  , (<<<)
  , (<>)
  , ($)
  )
import Crypto.Simple
  ( class Hashable
  , Digest
  , Hash(SHA256)
  , hash
  , toString
  ) as CS
import Data.Newtype (class Newtype, wrap, unwrap)
import Contract.PlutusData
  ( class ToData
  , PlutusData (Constr)
  , toData
  )
import Contract.Numeric.BigNum (zero)

newtype Hash = Hash CS.Digest

derive instance Eq Hash
derive instance Newtype Hash _

instance ToData Hash where
  toData h = Constr zero
    [ toData $ CS.toString $ unwrap h
    ] 

hash :: forall a. CS.Hashable a => a -> Hash
hash = wrap <<< CS.hash CS.SHA256

combineHash :: Hash -> Hash -> Hash
combineHash h h' = hash (CS.toString (unwrap h) <> CS.toString (unwrap h'))
