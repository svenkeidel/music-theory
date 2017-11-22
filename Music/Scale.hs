module Music.Scale(Scale,Tonic,fromIntervals,accumulative,majorPentatonic,minorPentatonic) where

import Music.Key
import Music.Interval

import Data.List (sort)
import Data.Foldable (toList)

type Tonic = Key
newtype Scale = Scale [Key]

instance Eq Scale where
  Scale s1 == Scale s2 = sort s1 == sort s2

instance Ord Scale where
  Scale s1 <= Scale s2 = all (`elem` s2) s1

instance Show Scale where
  show (Scale s1) = show (sort s1)

fromFoldable :: Foldable f => f Key -> Scale
fromFoldable = Scale . toList

fromIntervals :: (Functor f, Foldable f) => Tonic -> f Interval -> Scale
fromIntervals b intervals = fromFoldable $ (b +) . fromInt <$> intervals

accumulative :: Tonic -> [Interval] -> Scale
accumulative base intervals =
  fromFoldable $ scanl (\k iv -> k + fromInt iv) base intervals

majorPentatonic :: Tonic -> Scale
majorPentatonic b =
  accumulative
    b
    (replicate 4 perfectFifth)

minorPentatonic :: Tonic -> Scale
minorPentatonic b =
  accumulative
    (b - 3 * fromInt perfectFifth)
    (replicate 4 perfectFifth)
