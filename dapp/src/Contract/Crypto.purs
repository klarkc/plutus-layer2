module Contract.Crypto
  ( module Exports
  , Hash(..)
  , hash
  , combineHash
  ) where

import Control.Monad.Error.Class (liftMaybe)
import Crypto.Simple
  ( class Hashable
  ) as Exports
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Prelude
  ( class Eq
  , class Show
  , (<<<)
  , ($)
  , bind
  , pure
  )
import Crypto.Simple
  ( class Hashable
  , Digest
  , Hash(SHA256)
  , hash
  , toString
  , exportToBuffer
  , importFromBuffer
  ) as CS
import Contract.PlutusData
  ( class ToData
  , PlutusData (Constr)
  , toData
  )
import Contract.Numeric.BigNum (zero)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (take)
import Effect (Effect)
import Effect.Exception (error)
import Node.Buffer (concat)

newtype Hash = Hash CS.Digest

derive instance Eq Hash
derive instance Newtype Hash _
instance Show Hash where
  show = take 8 <<< hashToHex
instance ToData Hash where
  toData h = Constr zero
    [ toData $ hexToByteArrayUnsafe $ hashToHex h
    ] 

hash :: forall a. CS.Hashable a => a -> Hash
hash = wrap <<< CS.hash CS.SHA256

hashToHex :: Hash -> String
hashToHex = CS.toString <<< unwrap

combineHash :: Hash -> Hash -> Effect Hash
combineHash h h' = do
  let b  = CS.exportToBuffer $ unwrap h
      b' = CS.exportToBuffer $ unwrap h'
      err = error "failed to import buffer"
  bb <- concat [b, b']
  bb' :: CS.Digest <- liftMaybe err $ CS.importFromBuffer bb
  pure $ hash bb'
