{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Foldable     (toList)
import Data.Heap
import Data.MyPrelude
import Data.Nat.Binary   (Bin)
import Data.Nat.Peano    (Peano)
import Data.Proxy        (Proxy(..))

main :: IO ()
main = defaultMain
    [ bgroup "Peano" $ map sortBenchEnvPeano ls
    , bgroup "Bin"   $ map sortBenchEnvBin   ls
    ]

  where

    ls = [10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120]

sort' :: forall nat. Nat nat => Proxy nat -> [Natural] -> [Natural]
sort' Proxy = toList . toHeap @nat . map (\x -> (x, x))

sortBench :: Nat nat => Proxy nat -> String -> [Natural] -> Benchmark
sortBench p name xs = bench name $ nf (sort' p) xs

sortBenchEnv :: Nat nat => Proxy nat -> Natural -> String -> Benchmark
sortBenchEnv p len name = env (shuffle len) $ sortBench p name

sortBenchEnvPeano :: Natural -> Benchmark
sortBenchEnvPeano len =
    sortBenchEnv (Proxy :: Proxy Peano) len $ "Peano-" ++ show len

sortBenchEnvBin :: Natural -> Benchmark
sortBenchEnvBin len =
    sortBenchEnv (Proxy :: Proxy Bin) len $ "Bin-" ++ show len

shuffle :: Natural -> IO [Natural]
shuffle n = return $ map fromIntegral $  evalRand (replicateM (fromIntegral n) $ getRandomR (1 :: Int, 100)) $ mkStdGen 1234567
