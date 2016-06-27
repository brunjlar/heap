{-# LANGUAGE DataKinds #-}

module Data.HeapSpec (spec) where

import Test.Hspec
import Data.Foldable (toList)
import Data.Heap

spec :: Spec
spec = toHeapSpec

toHeapSpec :: Spec
toHeapSpec = describe "toHeap" $ do

    it "should create a proper heap" $ do 

        let xs = [(2, 'a'), (5, 'e'), (1, 'H'), (4, 'k'), (7, 'l'), (6, 'l'), (3, 's')]
        toList (toHeap xs) `shouldBe` "Haskell"
