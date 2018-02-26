{-# LANGUAGE OverloadedStrings #-}
module Music.Harmony where

import Prelude hiding (repeat)
import Music.Key
import Music.Chord
import Music.Interval
import Data.Text (Text)
import Data.RomanNumerals
import qualified Data.List as L
import Data.Monoid

data Song = Song { title :: Text, genre :: Text, harmony :: [Tonic] }

data Tonic = Tonic {tonic :: Key, chords :: [(Key,Chord)]}

relative :: Tonic -> [(Roman,Chord)]
relative (Tonic t ch) = [ (Roman $ fromEnum (interval t k) + 1, chord) | (k,chord) <- ch ]

lastChristmas :: Song
lastChristmas = Song {
    title = "Last Christmas",
    genre = "Christmas Music",
    harmony = [Tonic C $ intro <> twice (chorus <> solo <> verse) <> chorus <> coda]
  }
  where
    intro = [(C,major)]
    core =  [(C,major),(A,minor),(D,minor),(G,suspended4),(G,dominant7)]
    chorus = twice core
    solo = core
    verse = twice core
    coda = [(C,major)]

winterWonderland :: Song
winterWonderland = Song {
    title = "Winter Wonderland",
    genre = "Christmas Music",
    harmony = [Tonic F $ intro <> twice (chorus <> bridge <> core) <> coda]
  }
  where
    intro = [(C,major),(BFlat,major),(A,minor),(G,minor),(G,dominant7),(C,dominant7),(F,major)]
    core = [(F,major),(G,minor),(G,minor7),(C,dominant7),(F,major),(D,dominant7),(G,dominant7),(C,dominant7),(F,major)]
    chorus = twice core
    bridge = [(A,major),(B,minor),(CSharp,minor),(B,minor),(A,major),(B,minor),(CSharp,minor),
              (B,minor),(C,major),(D,minor),(E,minor),(D,minor),(D,dominant7),(G,dominant7),(C,dominant7)]
    coda = [(G,dominant7),(C,dominant7),(F,major)]

repeat :: Int -> [a] -> [a]
repeat n = L.concat . L.replicate n

twice :: [a] -> [a]
twice = repeat 2
