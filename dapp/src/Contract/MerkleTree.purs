module Contract.MerkleTree
  ( MerkleTree (..)
  , Proof
  , fromFoldable
  , rootHash
  , mkProof
  , member
  ) where

import Contract.Prelude
  ( class Generic
  , class Show
  , (/)
  , (-)
  , (==)
  , ($)
  , (>>=)
  , genericShow
  , bind
  , pure
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
import Effect (Effect)

data MerkleTree a
  = MerkleEmpty
  | MerkleNode Hash (MerkleTree a) (MerkleTree a)
  | MerkleLeaf Hash a

derive instance Generic (MerkleTree a) _
instance Show a => Show (MerkleTree a) where
  show t = genericShow t

type Proof = DL.List (Either Hash Hash)

rootHash :: forall a. MerkleTree a -> Hash
rootHash = case _ of
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ -> h

fromFoldable :: forall f a. Hashable a => Foldable f => f a -> Effect (MerkleTree a)
fromFoldable es = recursively (DL.length es') es'
 where
  es' = DL.fromFoldable es
  recursively len = case _ of
      DL.Nil -> pure MerkleEmpty
      DL.Cons e (DL.Nil) -> pure $ MerkleLeaf (hash e) e
      xs -> do
        let cutoff = len / 2
            l = DL.take cutoff xs
            r = DL.drop cutoff xs
        lnode <- recursively cutoff l
        rnode <- recursively (len - cutoff) r
        c <- rootHash lnode `combineHash` rootHash rnode
        pure $ MerkleNode c lnode rnode

mkProof :: forall a. Hashable a => a -> MerkleTree a -> Maybe Proof
mkProof e = go DL.Nil
 where
  he = hash e
  go es = case _ of
    MerkleEmpty -> Nothing
    MerkleLeaf h _ ->
      if h == he
        then Just es
        else Nothing
    MerkleNode _ l r ->
      go (DL.Cons (Right $ rootHash r) es) l <|> go (DL.Cons (Left $ rootHash l) es) r

member :: forall a. Hashable a => a -> Hash -> Proof -> Effect Boolean
member e root proof = go proof (hash e)
 where
  comb l r q = combineHash l r >>= go q
  go xs root' = case xs of
    DL.Nil -> pure $ root' == root
    DL.Cons (Left l) q -> comb l root' q
    DL.Cons (Right r) q -> comb root' r q
