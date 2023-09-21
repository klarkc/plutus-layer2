{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings #-}
module Validator.Trace
  ( trace
  ) where

import PlutusTx.Prelude
  ( BuiltinByteString
  , Bool(True, False)
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

trace :: (Datum -> Redeemer -> sc -> Bool) -> Datum -> Redeemer -> sc -> Bool
trace f dat red ctx = let ret = f (traceDatum dat) (traceRedeemer red) ctx
                   in case ret of
                        True -> PTX.trace "Success" ret
                        False -> PTX.trace "Failure" ret
{-# INLINEABLE trace #-}

traceDatum :: Datum -> Datum
traceDatum d
  = PTX.trace "/Datum"
  $ traceHash
  $ PTX.trace "Datum" d
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
      traceEither (Left h) = Left $ trace' "Left" h
      traceEither (Right h) = Right $ trace' "Right" h
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

