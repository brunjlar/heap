{-# LANGUAGE DataKinds #-}

module Data.HeapSpec (spec) where

import Test.Hspec
import Data.Foldable (toList)
import Data.Heap
import Numeric.Natural

spec :: Spec
spec = toHeapSpec

toHeapSpec :: Spec
toHeapSpec = describe "toHeap" $ do

    it "should allow to sort a short list" $ do 

        let xs = [(2, 'a'), (5, 'e'), (1, 'H'), (4, 'k'), (7, 'l'), (6, 'l'), (3, 's')]
        toList (toHeap xs) `shouldBe` "Haskell"

    it "should sort allow to sort a long list" $ do

        let n  = 2000
            xs = [(x, x) | x <- shuffle n]
        toList (toHeap xs) `shouldBe` [1 .. n]

shuffle :: Natural -> [Natural]
shuffle n = go [] [1 .. n] where

    go xs ys
        | null ys   = xs
        | otherwise = let (x : zs) = take 7 ys
                          zs'      = drop 7 ys
                          ys'      = zs' ++ zs
                      in  go (x : xs) ys'
