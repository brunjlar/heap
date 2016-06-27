{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Logic
    ( combine 
    , using
    ) where

import Data.Constraint

combine :: Dict a -> Dict b -> Dict (a, b)
combine Dict Dict = Dict

using :: Dict a -> (a => b) -> b
using d x = case d of Dict -> x
