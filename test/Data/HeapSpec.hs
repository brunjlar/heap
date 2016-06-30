{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.HeapSpec (spec) where

import Test.Hspec
import Data.Foldable     (toList)
import Data.Heap
import Data.MyPrelude
import Data.Nat.Binary   (Bin)
import Data.Nat.Peano    (Peano)
import Data.Utils.Random (shuffleR)
import Numeric.Natural

spec :: Spec
spec = do
    peanoSpec
    binSpec

peanoSpec :: Spec
peanoSpec = describe "Peano" $ do

    it "should allow to sort a short list" $ do 

        let xs = [(2, 'a'), (5, 'e'), (1, 'H'), (4, 'k'), (7, 'l'), (6, 'l'), (3, 's')]
        toList (toHeap @Peano xs) `shouldBe` "Haskell"

    it "should sort allow to sort a 2000-element list" $ do

        let n  = 2000
            xs = [(x, x) | x <- shuffle n]
        toList (toHeap @Peano xs) `shouldBe` [1 .. n]

binSpec :: Spec
binSpec = describe "Bin" $ do

    it "should allow to sort a short list" $ do 

        let xs = [(2, 'a'), (5, 'e'), (1, 'H'), (4, 'k'), (7, 'l'), (6, 'l'), (3, 's')]
        toList (toHeap @Bin xs) `shouldBe` "Haskell"

    it "should sort allow to sort a 2000-element list" $ do

        let n  = 2000
            xs = [(x, x) | x <- shuffle n]
        toList (toHeap @Bin xs) `shouldBe` [1 .. n]

    it "should sort allow to sort a 10000-element list" $ do

        let n  = 10000
            xs = [(x, x) | x <- shuffle n]
        toList (toHeap @Bin xs) `shouldBe` [1 .. n]
        
shuffle :: Natural -> [Natural]
shuffle n = evalRand (shuffleR [1 .. n]) $ mkStdGen 1234567
