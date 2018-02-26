{-# LANGUAGE OverloadedLists #-}
module Music.Scale.Heptatonic where

import Music.Interval
import Music.Scale
import Utils

majorDiatonic :: Scale
majorDiatonic = [2,2,1,2,2,2]

minorMelodic :: Scale
minorMelodic = [2,1,2,2,2,2]

minorHarmonic :: Scale
minorHarmonic = [2,1,2,2,1,3]

mode :: Int -> Scale -> Scale
mode i f = init (nth i rotate (intervalsLooped f))

ionian :: Scale -> Scale
ionian = mode 1

dorian :: Scale -> Scale
dorian = mode 2

phrygian :: Scale -> Scale
phrygian = mode 3

lydian :: Scale -> Scale
lydian = mode 4

mixolydian :: Scale -> Scale
mixolydian = mode 5

aeolian :: Scale -> Scale
aeolian = mode 6

locrian :: Scale -> Scale
locrian = mode 7
