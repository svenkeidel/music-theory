module Data.NGrams where

import Data.Trie (Trie)
import qualified Data.Trie as T
import qualified Data.Map as M
import Data.Maybe

type Frequency = Int

type NGrams a = Trie a Frequency

ngrams :: Show a => Int -> [a] -> [[a]]
ngrams n0 l0 =
  let grams = go n0 l0
  in take (length grams - (n0 - 1)) grams
  where
    go _ [] = []
    go n l@(_:as) = take n l : go n as

fromList :: (Ord a) => [[a]] -> NGrams a
fromList = foldr (T.insertWith (+) 1) T.empty

frequency :: (Num n, Ord a) => [a] -> NGrams a -> n
frequency w n = fromIntegral $ fromMaybe 0 $ T.lookup w n

onSuccessors :: (Num n, Ord a) => [a] -> (Frequency -> Int) -> NGrams a -> n
onSuccessors w f = fromIntegral . sum . fmap (f . fromMaybe 0 . T.value) . M.elems . T.successors w

freqSuccessors :: (Num n, Ord a) => [a] -> NGrams a -> n
freqSuccessors w = onSuccessors w id

successorsWithFrequency :: (Num n, Ord a) => [a] -> (Frequency -> Bool) -> NGrams a -> n
successorsWithFrequency w p = onSuccessors w (boolToInt . p)

onNGrams :: (Num n, Ord a) => Int -> (Frequency -> Int) -> NGrams a -> n
onNGrams n f = fromIntegral . T.foldLevel n (\s freq -> s + f freq) 0

ngramsWithFrequency :: (Num n, Ord a) => Int -> (Frequency -> Bool) -> NGrams a -> n
ngramsWithFrequency n predicate = onNGrams n (boolToInt . predicate)

allUniGrams :: (Num n, Ord a) => NGrams a -> n
allUniGrams = onNGrams 1 id

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0
