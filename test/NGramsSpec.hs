{-# LANGUAGE OverloadedStrings #-}
module NGramsSpec(main, spec) where

import qualified Data.NGrams as N
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
-- import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  it "ngrams" $ do
    N.ngrams 1 "abcdefg" `shouldBe` ["a","b","c","d","e","f","g"]
    N.ngrams 2 "abcdefg" `shouldBe` ["ab","bc","cd","de","ef","fg"]
    N.ngrams 3 "abcdefg" `shouldBe` ["abc","bcd","cde","def","efg"]
    N.ngrams 4 "abcdefg" `shouldBe` ["abcd","bcde","cdef","defg"]
    N.ngrams 5 "abcdefg" `shouldBe` ["abcde","bcdef","cdefg"]
    N.ngrams 6 "abcdefg" `shouldBe` ["abcdef","bcdefg"]
    N.ngrams 7 "abcdefg" `shouldBe` ["abcdefg"]
    N.ngrams 8 "abcdefg" `shouldBe` []
  
  it "frequency" $ do
    (N.frequency "a" ngrams :: Int) `shouldBe` 2
    (N.frequency "b" ngrams :: Int) `shouldBe` 1
    (N.frequency "c" ngrams :: Int) `shouldBe` 0
    (N.frequency "aa" ngrams :: Int) `shouldBe` 3
    (N.frequency "ab" ngrams :: Int) `shouldBe` 1
    (N.frequency "ac" ngrams :: Int) `shouldBe` 2
    (N.frequency "bc" ngrams :: Int) `shouldBe` 1
    (N.frequency "ba" ngrams :: Int) `shouldBe` 1

  it "frequency of successors" $ do
    (N.freqSuccessors "a" ngrams :: Int) `shouldBe` 6
    (N.freqSuccessors "b" ngrams :: Int) `shouldBe` 2
    (N.freqSuccessors "c" ngrams :: Int) `shouldBe` 0
    
  it "successors with frequency" $ do
    (N.successorsWithFrequency "a" (==1) ngrams :: Int) `shouldBe` 1
    (N.successorsWithFrequency "a" (==2) ngrams :: Int) `shouldBe` 1
    (N.successorsWithFrequency "a" (==3) ngrams :: Int) `shouldBe` 1
    (N.successorsWithFrequency "a" (==4) ngrams :: Int) `shouldBe` 0
    (N.successorsWithFrequency "a" (>=1) ngrams :: Int) `shouldBe` 3
                                                        
  it "ngrams with frequency" $ do
    (N.ngramsWithFrequency 2 (==1) ngrams :: Int) `shouldBe` 3
    (N.ngramsWithFrequency 2 (==2) ngrams :: Int) `shouldBe` 1
    (N.ngramsWithFrequency 2 (==3) ngrams :: Int) `shouldBe` 1
    (N.ngramsWithFrequency 2 (==4) ngrams :: Int) `shouldBe` 0

  it "all unigrams" $
    (N.allUniGrams ngrams :: Int) `shouldBe` 3

  where
    ngrams = N.fromList ["a","a","b","aa","aa","aa","ab","ac","bc","ac","ba"]
