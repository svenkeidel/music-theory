{-# LANGUAGE OverloadedLists #-}
module Music.Scale.Hexatonic where

import           Music.Scale.Heptatonic (majorDiatonic)
import           Music.Scale

majorHexatonic :: Scale
majorHexatonic = init . majorDiatonic

wholeTone :: Scale
wholeTone = [2,2,2,2,2]

augmented :: Scale
augmented = [3,1,3,1,3]

prometheus :: Scale
prometheus = [2,2,2,3,1]

majorBlues :: Scale
majorBlues = [2,1,1,3,2]

minorBlues :: Scale
minorBlues = [3,2,1,1,3]

tritone :: Scale
tritone = [1,3,2,1,3]
