module Contract.Layer2
  ( TxData
  , Tx
  , Tree
  , tx1
  , tx2
  , tx3
  , tree
  , root
  ) where

import Prelude
  ( (<$>)
  )
import Contract.Crypto (Hash)
import Contract.MerkleTree
  ( MerkleTree
  , fromFoldable
  , rootHash
  )
import Effect (Effect)

type TxData = String
type Tx = Hash
type Tree a = MerkleTree a

tx1 :: TxData
tx1 = "tx1Data"

tx2 :: TxData
tx2 = "tx2Data"

tx3 :: TxData
tx3 = "tx3Data"

tree :: Effect (Tree TxData)
tree = fromFoldable [ tx1, tx2, tx3 ]

root :: Effect Tx
root = rootHash <$> tree
