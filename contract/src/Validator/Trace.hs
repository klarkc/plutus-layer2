{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings #-}
module Validator.Trace
  ( trace
  ) where

import PlutusTx.Prelude
  ( BuiltinByteString
  , Either (Left, Right)
  , ($)
  , (<>)
  , decodeUtf8
  , fmap
  )
import qualified PlutusTx.Trace as PTX
import PlutusTx.MerkleTree
  ( Hash (Hash)
  , Proof
  )
import Validator.Types
  ( Redeemer(..)
  , Datum
  )

trace :: (Datum -> Redeemer -> a) -> Datum -> Redeemer -> a
trace f dat red = f (traceDatum dat) (traceRedeemer red)
{-# INLINEABLE trace #-}

traceDatum :: Datum -> Datum
traceDatum d
  = PTX.trace "/Datum_Hash"
  $ traceHash
  $ PTX.trace "Datum_Hash" d
{-# INLINEABLE traceDatum #-}

traceRedeemer :: Redeemer -> Redeemer
traceRedeemer r
  = PTX.trace "/Redeemer_Proof"
  $ (\r' -> r' { proof = traceProof (proof r') })
  $ PTX.trace "Redeemer_Proof"
  $ PTX.trace "/Redeemer_Element"
  $ (\r' -> r' { element = traceBuiltinByteString (element r') })
  $ PTX.trace "Redeemer_Element" r
{-# INLINEABLE traceRedeemer #-}

traceProof :: Proof -> Proof
traceProof proof
  = PTX.trace "/Proof"
  $ fmap traceEither
  $ PTX.trace "Proof" proof
    where
      trace' lr h
        = PTX.trace ("/" <> lr)
        $ traceHash
        $ PTX.trace lr h
      traceEither (Left h) = Left $ trace' "L" h
      traceEither (Right h) = Right $ trace' "R" h
{-# INLINEABLE traceProof #-}

traceHash :: Hash -> Hash
traceHash (Hash h)
  = Hash
  $ PTX.trace "/Hash"
  $ traceBuiltinByteString
  $ PTX.trace "Hash" h
{-# INLINEABLE traceHash #-}


traceBuiltinByteString :: BuiltinByteString -> BuiltinByteString
traceBuiltinByteString b = PTX.trace (decodeUtf8 b) b
{-# INLINEABLE traceBuiltinByteString #-}

