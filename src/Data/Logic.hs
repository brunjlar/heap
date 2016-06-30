{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Logic
    ( combine 
    , using
    , alternative
    , type IfThenElse
    ) where

import Data.Constraint

combine :: Dict a -> Dict b -> Dict (a, b)
combine Dict Dict = Dict

using :: Dict a -> (a => b) -> b
using d x = case d of Dict -> x
{-# INLINE using #-}

alternative :: Either (Dict a) (Dict b) -> (a => c) -> (b => c) -> c
alternative (Left  Dict) x _ = x
alternative (Right Dict) _ y = y
{-# INLINE alternative #-}

type family IfThenElse (a :: Bool) (b :: k) (c :: k) :: k where
    IfThenElse 'True  b _ = b
    IfThenElse 'False _ c = c
