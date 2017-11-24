{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
module Music.Chord where

import           Music.Interval
import           Music.Key

import           Data.Semigroup
import           Data.Text(Text)

import           GHC.Exts


type Chord = Key -> [Key]

instance IsList Chord where
  type Item Chord = Interval
  fromList ivs base = scanl (+:) base ivs
  toList f = intervals (f C)

instance Show Chord where
  show f = show (f C)

detect :: Chord -> [Text]
detect chord = case length (chord C) of
  3 -> compareChords [(major,"major"),(minor,"minor"),(diminished,"diminished"), (augmented,"augmented")]
  4 -> compareChords [(majorSixth,"major sixth"),(minorSixth,"minor sixth"),
                      (dominantSeventh,"dominant seventh"),(majorSeventh,"major seventh"),(diminishedSeventh,"diminished seventh"),(augmentedSeventh,"augmented seventh")]
  _ -> []
  where
    compareChords chords = [ name | (chord',name) <- chords, chord' C == chord C ]

major :: Chord
major = [MajorThird, MinorThird]

minor :: Chord
minor = [MinorThird, MajorThird]

diminished :: Chord
diminished = [MinorThird, MinorThird]

augmented :: Chord
augmented = [MajorThird, MajorThird]

majorSixth :: Chord
majorSixth = major <> [MajorSixth]

minorSixth :: Chord
minorSixth = minor <> [MajorSixth]

dominantSeventh :: Chord
dominantSeventh = major <> [MinorSeventh]

majorSeventh :: Chord
majorSeventh = major <> [MajorSeventh]

minorSeventh :: Chord
minorSeventh = minor <> [MinorThird]

diminishedSeventh :: Chord
diminishedSeventh = diminished <> [MinorSeventh]

augmentedSeventh :: Chord
augmentedSeventh = augmented <> [MinorSeventh]

dominantNinth :: Chord
dominantNinth = majorSeventh <> [majorNinth]
