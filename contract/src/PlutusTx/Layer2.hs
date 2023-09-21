{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module PlutusTx.Layer2
  ( TxData
  , Tx
  , tx1
  , tx2
  , tx3
  , tree
  , root
  ) where

import PlutusTx.MerkleTree
  ( MerkleTree
  , Hash
  , fromList
  , rootHash
  )

import PlutusTx.Prelude
  ( BuiltinByteString
  )

type Tree = MerkleTree
type TxData = BuiltinByteString
type Tx = Hash

tx1 :: TxData
tx1 = "tx1Data"

tx2 :: TxData
tx2 = "tx2Data"

tx3 :: TxData
tx3 = "tx3Data"

tree :: Tree
tree = fromList [ tx1, tx2, tx3 ]

root :: Tx
root = rootHash tree
