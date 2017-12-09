module Music.Pitch where

import Music.Key

type Octave = Int

data Pitch = Pitch Octave Key
  deriving (Eq,Show,Ord)

instance Enum Pitch where
  fromEnum (Pitch o k) = o * 12 + fromEnum k
  toEnum n = let (o,k) = divMod n 12 in Pitch o (toEnum k)

instance Num Pitch where
  (+) = withNum2 (+)
  (*) = withNum2 (*)
  (-) = withNum2 (-)
  negate = withNum1 negate
  abs = withNum1 abs
  signum = withNum1 signum
  fromInteger = toEnum . fromInteger
