module Contract.MerkleTree
  ( MerkleTree (..)
  , Proof
  , fromFoldable
  ) where

import Prelude
  ( (/)
  , (-)
  , (<>)
  )
import Crypto.Simple as CS
import Data.Either (Either)
import Data.List as DL
import Data.Foldable (class Foldable)
import Data.Semigroup (class Semigroup)

type Hash = CS.Digest

data MerkleTree a
  = MerkleEmpty
  | MerkleNode Hash (MerkleTree a) (MerkleTree a)
  | MerkleLeaf Hash a

type Proof = DL.List (Either Hash Hash)

hash = CS.hash CS.SHA256

combineHash :: forall a. Semigroup a => CS.Hashable a => a -> a -> Hash
combineHash h h' = hash (h <> h')

fromFoldable :: forall f a. CS.Hashable a => Foldable f => f a -> MerkleTree a
fromFoldable es = recursively (DL.length es') es'
 where
  es' = DL.fromFoldable es
  recursively len = \l -> case l of
      DL.Nil -> MerkleEmpty
      DL.Cons e (DL.Nil) -> MerkleLeaf (hash e) e
      es ->
        let cutoff = len / 2
            l = DL.take cutoff es
            r = DL.drop cutoff es
            lnode = recursively cutoff l
            rnode = recursively (len - cutoff) r
            c = combineHash (rootHash lnode) (rootHash rnode)
         in MerkleNode c lnode rnode
