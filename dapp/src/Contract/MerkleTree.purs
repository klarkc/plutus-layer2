module Contract.MerkleTree
  ( MerkleTree (..)
  , Proof
  , fromFoldable
  , rootHash
  , mkProof
  ) where

import Prelude
  ( (/)
  , (-)
  , (==)
  , ($)
  )
import Control.Alt ((<|>))
import Contract.Crypto
  ( class Hashable
  , Hash
  , hash
  , combineHash
  )
import Data.Either (Either (Left, Right))
import Data.List
  ( List (Cons, Nil)
  , fromFoldable
  , take
  , drop
  , length
  ) as DL
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(Just, Nothing))

data MerkleTree a
  = MerkleEmpty
  | MerkleNode Hash (MerkleTree a) (MerkleTree a)
  | MerkleLeaf Hash a

type Proof = DL.List (Either Hash Hash)

rootHash :: forall a. MerkleTree a -> Hash
rootHash = \t -> case t of
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ -> h

fromFoldable :: forall f a. Hashable a => Foldable f => f a -> MerkleTree a
fromFoldable es = recursively (DL.length es') es'
 where
  es' = DL.fromFoldable es
  recursively len = \ls -> case ls of
      DL.Nil -> MerkleEmpty
      DL.Cons e (DL.Nil) -> MerkleLeaf (hash e) e
      xs ->
        let cutoff = len / 2
            l = DL.take cutoff xs
            r = DL.drop cutoff xs
            lnode = recursively cutoff l
            rnode = recursively (len - cutoff) r
            c = rootHash lnode `combineHash` rootHash rnode
         in MerkleNode c lnode rnode

mkProof :: forall a. Hashable a => a -> MerkleTree a -> Maybe Proof
mkProof e = go DL.Nil
 where
  he = hash e
  go es = \tx -> case tx of
    MerkleEmpty -> Nothing
    MerkleLeaf h _ ->
      if h == he
        then Just es
        else Nothing
    MerkleNode _ l r ->
      go (DL.Cons (Right $ rootHash r) es) l <|> go (DL.Cons (Left $ rootHash l) es) r
