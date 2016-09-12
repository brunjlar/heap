{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module HAL.Peano where

import Data.Constraint
import Numeric.Natural (Natural)

data Peano = Z | S Peano deriving (Show, Read, Eq)

infix 4 ??

type family (m :: Peano) ?? (n :: Peano) :: Ordering where
    'Z   ?? 'Z   = 'EQ
    'Z   ?? _    = 'LT
    'S _ ?? 'Z   = 'GT
    'S m ?? 'S n = m ?? n

type (m :: Peano) < (n :: Peano) = (m ?? n) ~ 'LT

data SingPeano :: Peano -> * where

    SZ :: SingPeano 'Z
    SS :: SingPeano n -> SingPeano ('S n)

ltS :: SingPeano n -> Dict (n < 'S n)
ltS SZ = Dict
ltS (SS n) = ltS n

data Heap a = Empty | Node !Natural !Natural a (Heap a) (Heap a) deriving (Show, Functor)

rank :: Heap a -> Natural
rank Empty            = 0
rank (Node _ r _ _ _) = r

priority :: Heap a -> Maybe Natural
priority Empty            = Nothing
priority (Node p _ _ _ _) = Just p

singleton :: Natural -> a -> Heap a
singleton p x = Node p 1 x Empty Empty

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
