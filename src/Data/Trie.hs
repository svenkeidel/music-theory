{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Trie where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

data Trie k v = Node { value :: !(Maybe v), children :: Map k (Trie k v) }
  deriving (Functor,Foldable,Traversable)

empty :: Trie k v
empty = Node Nothing M.empty

insertWith :: Ord k => (v -> v -> v) -> v -> [k] -> Trie k v -> Trie k v
insertWith f v ls (Node v' cs) = case ls of
  [] -> Node (Just (maybe v (f v) v')) cs
  (k:ks) -> Node v' (M.insertWith (\_ t -> insertWith f v ks t) k (insertWith f v ks empty) cs)

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup k0 (Node v cs) = case k0 of
  [] -> v
  (k:ks) -> M.lookup k cs >>= lookup ks

successors :: Ord k => [k] -> Trie k v -> Map k (Trie k v)
successors k0 (Node _ cs) = case k0 of
  [] -> cs
  (k:ks) -> fromMaybe M.empty (successors ks <$> M.lookup k cs)

foldLevel :: Int -> (a -> v -> a) -> a -> Trie k v -> a
foldLevel 0 f a (Node v _) = maybe a (f a) v
foldLevel n f a (Node _ cs) = foldl (foldLevel (n-1) f) a $ M.elems cs
