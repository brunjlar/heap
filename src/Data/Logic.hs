{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Logic
    ( combine 
    , using
    , alternative
    ) where

import Data.Constraint

combine :: Dict a -> Dict b -> Dict (a, b)
combine Dict Dict = Dict

using :: Dict a -> (a => b) -> b
using d x = case d of Dict -> x

alternative :: Either (Dict a) (Dict b) -> (a => c) -> (b => c) -> c
alternative (Left  Dict) x _ = x
alternative (Right Dict) _ y = y
