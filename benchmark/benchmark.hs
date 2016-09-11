{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Criterion.Main
import           Data.Foldable       (toList)
import           Data.Heap
import qualified Data.Heap.Unchecked as U
import           Data.MyPrelude
import           Data.Nat.Binary     (Bin)
import           Data.Nat.Peano      (Peano)
import           Data.Proxy          (Proxy(..))

main :: IO ()
main = defaultMain
    [ bgroup "Peano"     $ map sortBenchEnvPeano     ls
    , bgroup "Bin"       $ map sortBenchEnvBin       ls
    , bgroup "Unchecked" $ map sortBenchEnvUnchecked ls
    ]

  where

    ls = [250, 500 .. 2500]

sort' :: forall nat. Nat nat => Proxy nat -> [Natural] -> [Natural]
sort' Proxy = toList . toHeap @nat . map (\x -> (x, x))

sortUnchecked :: [Natural] -> [Natural]
sortUnchecked = toList . U.toHeap . map (\x -> (x, x))

sortBench :: Nat nat => Proxy nat -> String -> [Natural] -> Benchmark
sortBench p name xs = bench name $ nf (sort' p) xs

sortBenchUnchecked :: String -> [Natural] -> Benchmark
sortBenchUnchecked name xs = bench name $ nf (sortUnchecked) xs

sortBenchEnv :: Nat nat => Proxy nat -> Natural -> Benchmark
sortBenchEnv p len = env (shuffle len) $ sortBench p $ show len

sortBenchEnvPeano :: Natural -> Benchmark
sortBenchEnvPeano = sortBenchEnv (Proxy :: Proxy Peano)

sortBenchEnvBin :: Natural -> Benchmark
sortBenchEnvBin = sortBenchEnv (Proxy :: Proxy Bin)

sortBenchEnvUnchecked :: Natural -> Benchmark
sortBenchEnvUnchecked len = env (shuffle len) $ sortBenchUnchecked $ show len

shuffle :: Natural -> IO [Natural]
shuffle n = return $ map fromIntegral $  evalRand (replicateM (fromIntegral n) $ getRandomR (1 :: Int, 100)) $ mkStdGen 1234567
