{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module PlutusTx.Layer2
  ( TxData
  , Tx
  , tx1
  , tx2
  , tx3
  , tree
  ) where

import PlutusTx.MerkleTree
  ( MerkleTree
  , Hash
  , fromList
  )

import PlutusTx.Prelude
  ( BuiltinByteString
  )

type TxData = BuiltinByteString
type Tx = Hash

tx1 :: TxData
tx1 = "tx1Data"

tx2 :: TxData
tx2 = "tx2Data"

tx3 :: TxData
tx3 = "tx3Data"

tree :: MerkleTree
tree = fromList [ tx1, tx2, tx3 ]
