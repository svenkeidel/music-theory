module Music.Interval where

import Music.Key

-- Distance in frequency between two keys.
data Interval
  = Unison
  | MinorSecond | MajorSecond
  | MinorThird | MajorThird
  | PerfectFourth
  | Tritone
  | PerfectFifth
  | MinorSixth | MajorSixth
  | MinorSeventh | MajorSeventh
  deriving Show



interval :: Key -> Key -> Interval
interval k1 k2 = toEnum (fromEnum k2 - fromEnum k1)

intervals :: [Key] -> [Interval]
intervals l = zipWith interval l (tail l)

intervalsLooped :: [Interval] -> [Interval]
intervalsLooped l = l ++ [12 - sum l]

(@:) :: [Interval] -> Key -> [Key]
(@:) ivs base = scanl (+:) base ivs

(+:) :: Key -> Interval -> Key
(+:) k i = toEnum (fromEnum k + fromEnum i)

(-:) :: Key -> Interval -> Key
(-:) k i = toEnum (fromEnum k - fromEnum i)


instance Enum Interval where
  toEnum n = case n of
    0  -> Unison
    1  -> MinorSecond 
    2  -> MajorSecond
    3  -> MinorThird
    4  -> MajorThird
    5  -> PerfectFourth
    6  -> Tritone
    7  -> PerfectFifth
    8  -> MinorSixth
    9  -> MajorSixth
    10 -> MinorSeventh
    11 -> MajorSeventh
    _ -> toEnum $ (n `mod` 12) + (if n < 0 then 12 else 0)
  fromEnum iv = case iv of
    Unison        -> 0
    MinorSecond   -> 1
    MajorSecond   -> 2
    MinorThird    -> 3
    MajorThird    -> 4
    PerfectFourth -> 5
    Tritone       -> 6
    PerfectFifth  -> 7
    MinorSixth    -> 8
    MajorSixth    -> 9
    MinorSeventh  -> 10
    MajorSeventh  -> 11

instance Num Interval where
  (+) = withNum2 (+)
  (*) = withNum2 (*)
  (-) = withNum2 (-)
  negate = withNum1 negate
  abs = withNum1 abs
  signum = withNum1 signum
  fromInteger = toEnum . fromInteger

perfectOctave :: Interval
perfectOctave = toEnum 12

minorNinth :: Interval
minorNinth = toEnum 13

majorNinth :: Interval
majorNinth = toEnum 14
