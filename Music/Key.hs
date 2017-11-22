module Music.Key where

import Data.Hashable

data Key = C | CSharp | DFlat
         | D | DSharp | EFlat
         | E
         | F | FSharp | GFlat
         | G | GSharp | AFlat
         | A | ASharp | BFlat
         | B

instance Show Key where
  show k = case k of
    C -> "C"
    CSharp -> "C♯"; DFlat -> "D♭"
    D -> "D"
    DSharp -> "D♯"; EFlat -> "E♭"
    E -> "E"
    F -> "F"
    FSharp -> "F♯"; GFlat -> "G♭"
    G -> "G"
    GSharp -> "G♯"; AFlat -> "A♭"
    A -> "A"
    ASharp -> "A♯"; BFlat -> "B♭"
    B -> "B"
 
instance Eq Key where
  k1 == k2 = fromEnum k1 == fromEnum k2

instance Ord Key where
  k1 <= k2 = fromEnum k1 <= fromEnum k2

instance Enum Key where
  toEnum n = case n of
    0 -> C
    1 -> CSharp
    2 -> D
    3 -> DSharp
    4 -> E
    5 -> F
    6 -> FSharp
    7 -> G
    8 -> GSharp
    9 -> A
    10 -> ASharp
    11 -> B
    _ -> toEnum $ (n `mod` 12) + (if n < 0 then 12 else 0)
  fromEnum k = case k of
    C -> 0
    CSharp -> 1; DFlat -> 1
    D -> 2
    DSharp -> 3; EFlat -> 3
    E -> 4
    F -> 5
    FSharp -> 6; GFlat -> 6
    G -> 7
    GSharp -> 8; AFlat -> 8
    A -> 9
    ASharp -> 10; BFlat -> 10
    B -> 11

instance Bounded Key where
  minBound = C
  maxBound = B

instance Num Key where
  (+) = withNum2 (+)
  (*) = withNum2 (*)
  (-) = withNum2 (-)
  negate = withNum1 negate
  abs = withNum1 abs
  signum = withNum1 signum
  fromInteger = toEnum . fromInteger

instance Hashable Key where
  hashWithSalt s k = hashWithSalt s (fromEnum k)

flatten :: Key -> Key
flatten k = case k of
  CSharp -> DFlat
  DSharp -> EFlat
  FSharp -> GFlat
  ASharp -> BFlat
  _ -> k

sharpen :: Key -> Key
sharpen k = case k of
  DFlat -> CSharp
  EFlat -> DSharp
  GFlat -> FSharp
  BFlat -> ASharp
  _ -> k

withNum1 :: (Int -> Int) -> Key -> Key
withNum1 f = toEnum . f . fromEnum

withNum2 :: (Int -> Int -> Int) -> Key -> Key -> Key
withNum2 f k1 k2 = toEnum (f (fromEnum k1) (fromEnum k2))
