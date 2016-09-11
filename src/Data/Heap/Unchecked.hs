{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Data.Heap.Unchecked
Description : weakly typed heaps
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides weakly typed leftist 'Heap's and operations on such heaps.
Neither the /heap property/ nor the /leftist property/ are checked at compile time
and have to be maintained by programming discipline.
-}

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

-- | Weakly typed leftist heap, where each node carries a payload of type @a@ and a priority of type @'Natural'@.
--
data Heap a = Empty | Node !Natural !Natural a (Heap a) (Heap a) deriving (Show, Functor)

-- | Gives the /rank/ of the 'Heap', i.e. the length of the 'Heap' \'s right spine.
--
rank :: Heap a -> Natural
rank Empty            = 0
rank (Node _ r _ _ _) = r

-- | Gives the priority of the (root of the) 'Heap' (or 'Nothing' for the empty 'Heap').
--
priority :: Heap a -> Maybe Natural
priority Empty            = Nothing
priority (Node p _ _ _ _) = Just p

-- | Constructs a 'Heap' with one node, given a priority and a payload.
--
singleton :: Natural -> a -> Heap a
singleton p x = Node p 1 x Empty Empty

-- | Merges two 'Heap's into one.
--
merge :: Heap a -> Heap a -> Heap a
merge Empty                h'                  = h'
merge h                    Empty               = h
merge h@(Node p _ x ys zs) h'@(Node q _ _ _ _)
    | q < p                                    = merge h' h
    | otherwise                                =
        let h''@(Node _ r _ _ _) = merge zs h'
            r'                   = rank ys
        in if r <= r'
              then Node p (succ r ) x ys  h''
              else Node p (succ r') x h'' ys

instance Monoid (Heap a) where

    mempty = Empty

    mappend = merge

-- | Constructs a 'Heap' from a collection of priority-payload pairs.
--
toHeap :: Foldable f => f (Natural, a) -> Heap a
toHeap = foldMap (uncurry singleton)

-- | Inserts a payload with specified priority into a heap.
--
insert :: Natural -> a -> Heap a -> Heap a
insert p = mappend . singleton p

-- | Tries to pop the minimal-priority element from the 'Heap'
--   and to return that element and a 'Heap', consisting of the reamining elements.
--   For the empty 'Heap', this function returns 'Nothing'.
--
pop :: Heap a -> Maybe (Natural, a, Heap a)
pop Empty              = Nothing
pop (Node p _ x ys zs) = Just (p, x, ys <> zs)

-- | Like 'pop', but only returns the minimal-priority element.
--
peek :: Heap a -> Maybe (Natural, a)
peek h = pop h >>= \(p, x, _) -> return (p, x)

instance Foldable Heap where

    foldMap f = go mempty where

        go m h = case pop h of
            Nothing         -> m
            Just (_, x, h') -> go (m <> f x) h'
