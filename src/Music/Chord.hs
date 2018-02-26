{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
module Music.Chord where

import           Music.Interval
import           Data.Semigroup

type Chord = [Interval]

-- detect :: Chord -> [Text]
-- detect chord = case length (chord C) of
--   3 -> compareChords [(major,"major"),(minor,"minor"),(diminished,"diminished"), (augmented,"augmented")]
--   4 -> compareChords [(majorSixth,"major sixth"),(minorSixth,"minor sixth"),
--                       (dominantSeventh,"dominant seventh"),(majorSeventh,"major seventh"),
--                       (diminishedSeventh,"diminished seventh"),(augmentedSeventh,"augmented seventh")]
--   _ -> []
--   where
--     compareChords chords = [ name | (chord',name) <- chords, chord' C == chord C ]

major :: Chord
major = [MajorThird, MinorThird]

minor :: Chord
minor = [MinorThird, MajorThird]

diminished :: Chord
diminished = [MinorThird, MinorThird]

augmented :: Chord
augmented = [MajorThird, MajorThird]

suspended2 :: Chord
suspended2 = [MajorSecond, PerfectFourth]

suspended4 :: Chord
suspended4 = [PerfectFourth, MajorSecond]

major6 :: Chord
major6 = major <> [MajorSecond]

minor6 :: Chord
minor6 = minor <> [MajorSecond]

dominant7 :: Chord
dominant7 = major <> [MinorThird]

major7 :: Chord
major7 = major <> [MajorThird]

minor7 :: Chord
minor7 = minor <> [MinorThird]

diminished7 :: Chord
diminished7 = diminished <> [MinorThird]

augmented7 :: Chord
augmented7 = augmented <> [MinorSeventh]
