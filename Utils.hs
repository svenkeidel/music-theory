{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Utils where

import           Prelude hiding (head,last,map,init,zipWith)

import           GHC.Exts

-- | Iterate a function n times
nth :: Int -> (a -> a) -> (a -> a)
nth 1 _ = id
nth n f = f . nth (pred n) f

rotate :: IsList l => l -> l
rotate l = fromList $ case toList l of
  (a:as) -> as ++ [a]
  [] -> []
