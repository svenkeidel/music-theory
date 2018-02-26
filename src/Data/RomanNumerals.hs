{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RomanNumerals where

import Text.Numeral.Roman

newtype Roman = Roman Int
  deriving (Ord,Num,Eq)

instance Show Roman where
  show (Roman n) = toRoman n
