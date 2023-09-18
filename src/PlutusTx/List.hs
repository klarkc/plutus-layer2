{-# LANGUAGE NoImplicitPrelude #-}
module PlutusTx.List
  ( module PlutusTx.List
  , drop
  , take
  ) where

import PlutusTx.Builtins (Integer)
import PlutusTx.Ord ((<=))
import qualified PlutusTx.Builtins as Builtins

{-# INLINABLE take #-}
-- | Plutus Tx version of 'Data.List.take'.
take :: Integer -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (Builtins.subtractInteger n 1) xs

{-# INLINABLE drop #-}
-- | Plutus Tx version of 'Data.List.drop'.
drop :: Integer -> [a] -> [a]
drop n xs     | n <= 0 = xs
drop _ []              = []
drop n (_:xs)          = drop (Builtins.subtractInteger n 1) xs
