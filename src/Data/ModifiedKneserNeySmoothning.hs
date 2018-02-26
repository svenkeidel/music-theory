module Data.ModifiedKneserNeySmoothning where

import Data.NGrams

data Discounts = Discounts { d1 :: Double, d2 :: Double, d3 :: Double }

discounts :: Ord a => Int -> NGrams a -> Discounts
discounts n grams = Discounts {
    d1 = 1 - 2 * y * n2 / n1, 
    d2 = 2 - 3 * y * n3 / n2,
    d3 = 3 - 4 * y * n4 / n3
  }
  where
    n1 = ngramsWithFrequency n (== 1) grams
    n2 = ngramsWithFrequency n (== 2) grams
    n3 = ngramsWithFrequency n (== 3) grams
    n4 = ngramsWithFrequency n (== 4) grams
    y = n1 / (n1 + 2 * n2)

discount :: Discounts -> Int -> Double
discount dis n = case n of
  0 -> 0
  1 -> d1 dis
  2 -> d2 dis
  _ -> d3 dis

probability :: Ord a => [Discounts] -> NGrams a -> a -> [a] -> Double
probability _ grams wi [] = frequency [wi] grams / allUniGrams grams
probability (dis:rest) grams wi wn =
  let w = wn ++ [wi]
  in (frequency w grams - discount dis (frequency w grams))
      / freqSuccessors wn grams +
     (d1 dis * successorsWithFrequency wn (==1) grams +
      d2 dis * successorsWithFrequency wn (==2) grams +
      d3 dis * successorsWithFrequency wn (>=3) grams)
      / freqSuccessors wn grams
      * probability rest grams wi (init wn)
probability _ _ _ _ = error "not enough discount values"


