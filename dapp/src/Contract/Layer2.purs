module Contract.Layer2
  ( TxData
  , Tx
  , tx1
  , tx2
  , tx3
  , tree
  ) where

import Contract.Crypto (Hash)
import Contract.MerkleTree (MerkleTree, fromFoldable)
import Effect (Effect)

type TxData = String
type Tx = Hash

tx1 :: TxData
tx1 = "tx1Data"

tx2 :: TxData
tx2 = "tx2Data"

tx3 :: TxData
tx3 = "tx3Data"

tree :: Effect (MerkleTree TxData)
tree = fromFoldable [ tx1, tx2, tx3 ]
