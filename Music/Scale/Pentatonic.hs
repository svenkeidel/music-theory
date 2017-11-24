module Music.Scale.Pentatonic where

import Music.Scale
import Music.Interval
import Utils

import Data.List
import GHC.Exts

majorPentatonic :: Scale
majorPentatonic =
  reorder $ fromList $ replicate 4 PerfectFifth

minorPentatonic :: Scale
minorPentatonic =
  reorder (\b -> majorPentatonic (b -: (3 * PerfectFifth)))

reorder :: Scale -> Scale
reorder f k = go (sort (f k))
  where
    go s | head s == k = s
         | otherwise = go (rotate s)
