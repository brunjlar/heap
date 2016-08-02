module Main where

import Criterion.Main
import Data.Heap

main :: IO ()
main = defaultMain
    [
    ]
    {-
    [ bgroup "white"
        [ bench "10/200"     $ whnf (w   10)   200
        , bench "10/2000"    $ whnf (w   10)  2000
        , bench "10/20000"   $ whnf (w   10) 20000
        , bench "100/200"    $ whnf (w  100)   200
        , bench "100/2000"   $ whnf (w  100)  2000
        , bench "100/20000"  $ whnf (w  100) 20000
        , bench "1000/200"   $ whnf (w 1000)   200
        , bench "1000/2000"  $ whnf (w 1000)  2000
        , bench "1000/20000" $ whnf (w 1000) 20000
        ]
    , env setupEnv $ \ ~(m, xss) -> bgroup "linear"
        [ l m xss  1 5
        , l m xss  5 5
        , l m xss 10 5
        ]
    ]
    -}
