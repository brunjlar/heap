{-# LANGUAGE DeriveFunctor #-}

module Data.Heap.Unchecked
    ( Heap
    , rank
    , priority
    , singleton
    , (<>)
    , toHeap
    , insert
    , pop
    , peek
    ) where

import Data.Monoid     ((<>))
import Numeric.Natural

data Heap a = Empty | Tree !Natural !Natural a (Heap a) (Heap a) deriving (Show, Functor)

rank :: Heap a -> Natural
rank Empty            = 0
rank (Tree _ r _ _ _) = r

priority :: Heap a -> Maybe Natural
priority Empty            = Nothing
priority (Tree p _ _ _ _) = Just p

singleton :: Natural -> a -> Heap a
singleton p x = Tree p 1 x Empty Empty

merge :: Heap a -> Heap a -> Heap a
merge Empty                h'                  = h'
merge h                    Empty               = h
merge h@(Tree p _ x ys zs) h'@(Tree q _ _ _ _)
    | q < p                                    = merge h' h
    | otherwise                                =
        let h''@(Tree _ r _ _ _) = merge zs h'
            r'                   = rank ys
        in if r <= r'
              then Tree p (succ r ) x ys  h''
              else Tree p (succ r') x h'' ys

instance Monoid (Heap a) where

    mempty = Empty

    mappend = merge

toHeap :: Foldable f => f (Natural, a) -> Heap a
toHeap = foldMap (uncurry singleton)

insert :: Natural -> a -> Heap a -> Heap a
insert p = mappend . singleton p

pop :: Heap a -> Maybe (Natural, a, Heap a)
pop Empty              = Nothing
pop (Tree p _ x ys zs) = Just (p, x, ys <> zs)

peek :: Heap a -> Maybe (Natural, a)
peek h = pop h >>= \(p, x, _) -> return (p, x)

instance Foldable Heap where

    foldMap f = go mempty where

        go m h = case pop h of
            Nothing         -> m
            Just (_, x, h') -> go (m <> f x) h'
