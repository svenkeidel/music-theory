{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
module Music.Chord where

import           Music.Interval
import           Music.Key

import           Control.Monad

import qualified Data.HashSet as S
import           Data.Hashable(Hashable(..))
import qualified Data.List as L
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T

import           GHC.Exts

type Tonic = Key
newtype Chord = Chord [Key]

instance Eq Chord where
  Chord s1 == Chord s2 = L.sort s1 == L.sort s2

instance Ord Chord where
  Chord s1 <= Chord s2 = all (`elem` s2) s1

instance Show Chord where
  show (Chord s1) = show s1

instance Semigroup Chord where
  Chord s1 <> Chord s2 = Chord (s1 <> s2)

instance Hashable Chord where
  hashWithSalt s (Chord c) = hashWithSalt s c

instance IsList Chord where
  type Item Chord = Key
  fromList = Chord
  toList = keys

instance IsList (Tonic -> Chord) where
  type Item (Tonic -> Chord) = Interval
  fromList ivs b = fromList $ addInterval b <$> ivs
  toList k = intervals (k C)

accumulative :: [Interval] -> (Tonic -> Chord)
accumulative ivs base =
  fromList $ scanl addInterval base ivs

inversions :: Chord -> [Chord]
inversions (Chord c) = Chord <$> take (length c) (iterate rotate c)
  where
    rotate :: [a] -> [a]
    rotate (a:as) = as ++ [a]
    rotate [] = []

keys :: Chord -> [Key]
keys (Chord c) = c

intervals :: Chord -> [Interval]
intervals c = zipWith interval (toList c) (tail (toList c))

detect :: Chord -> [Text]
detect (Chord c) = case length c of
  3 -> compareChords [(major,"major"),(minor,"minor"),(diminished,"diminished"), (augmented,"augmented")]
  4 -> compareChords [(majorSixth,"major sixth"),(minorSixth,"minor sixth"),
                      (dominantSeventh,"dominant seventh"),(majorSeventh,"major seventh"),(diminishedSeventh,"diminished seventh"),(augmentedSeventh,"augmented seventh")]
  _ -> []
  where
    compareChords chords = do
      (f,name) <- chords
      let k = head c
      guard $ f k `S.member` S.fromList (inversions (Chord c))
      return $ T.unwords [T.pack (show k), name]

major :: Tonic -> Chord
major = accumulative [MajorThird, MinorThird]

minor :: Tonic -> Chord
minor = accumulative [MinorThird, MajorThird]

diminished :: Tonic -> Chord
diminished = accumulative [MinorThird, MinorThird]

augmented :: Tonic -> Chord
augmented = accumulative [MajorThird, MajorThird]

majorSixth :: Tonic -> Chord
majorSixth = major <> [MajorSixth]

minorSixth :: Tonic -> Chord
minorSixth = minor <> [MajorSixth]

dominantSeventh :: Tonic -> Chord
dominantSeventh = major <> [MinorSeventh]

majorSeventh :: Tonic -> Chord
majorSeventh = major <> [MajorSeventh]

minorSeventh :: Tonic -> Chord
minorSeventh = minor <> [MinorThird]

diminishedSeventh :: Tonic -> Chord
diminishedSeventh = diminished <> [MinorSeventh]

augmentedSeventh :: Tonic -> Chord
augmentedSeventh = augmented <> [MinorSeventh]

dominantNinth :: Tonic -> Chord
dominantNinth = majorSeventh <> [majorNinth]
