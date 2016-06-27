{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}

module Data.Nat
    ( Nat(..)
    , type (<=?)
    , type (<=)
    , type Min
    , SNat(..)
    , SNAT(..)
    , toNatural
    , toSNAT
    , leqGeqEq
    , leqGeqDec
    , leqGtDec
    , geqMin
    , minProd
    , minSymm
    ) where

import Data.Constraint
import Data.Logic
import Numeric.Natural

data Nat = Z | S Nat deriving (Show, Read, Eq)

infix 4 <, <=, >=, >, <=?

type family (m :: Nat) <=? (n :: Nat) :: Bool where
    'Z   <=? _    = 'True
    'S _ <=? 'Z   = 'False
    'S m <=? 'S n = m <=? n

type (m :: Nat) <= (n :: Nat) = (m <=? n) ~ 'True

type (m :: Nat) >= (n :: Nat) = n <= m

type (m :: Nat) < (n :: Nat) = (n <=? m) ~ 'False

type (m :: Nat) > (n :: Nat) = n < m

type Min (m :: Nat) (n :: Nat) = IfThenElse (m <=? n) m n

data SNat :: Nat -> * where

    SZ :: SNat 'Z

    SS :: SNat n -> SNat ('S n)

data SNAT :: * where

    SNAT :: SNat n -> SNAT

toSNAT :: Natural -> SNAT
toSNAT 0 = SNAT SZ
toSNAT n = case toSNAT (pred n) of
    SNAT n' -> SNAT (SS n')

toNatural :: SNat n -> Natural
toNatural SZ     = 0
toNatural (SS n) = succ $ toNatural n

instance Show (SNat n) where

    show = show . toNatural

instance Show SNAT where

    show (SNAT n) = show n

leqGeqEq :: (m <= n, m >= n) => SNat m -> SNat n -> Dict (m ~ n)
leqGeqEq SZ SZ = Dict
leqGeqEq (SS m) (SS n) = using (leqGeqEq m n) Dict

leqGeqDec :: SNat m -> SNat n -> Either (Dict (m <= n)) (Dict (m >= n))
leqGeqDec SZ     _      = Left Dict
leqGeqDec _      SZ     = Right Dict
leqGeqDec (SS m) (SS n) = leqGeqDec m n

leqGtDec :: SNat m -> SNat n -> Either (Dict (m <= n)) (Dict (m > n))
leqGtDec SZ     _      = Left Dict
leqGtDec (SS _) SZ     = Right Dict
leqGtDec (SS m) (SS n) = leqGtDec m n

geqMin :: (m >= n) => SNat m -> SNat n -> Dict (Min m n ~ n)
geqMin m n = alternative (leqGtDec m n)
    (using (leqGeqEq m n) Dict)
    Dict

minProd :: (l <= m, l <= n) => SNat l -> SNat m -> SNat n -> Dict (l <= Min m n)
minProd _ m n = alternative (leqGeqDec m n)
    Dict
    (using (geqMin m n) Dict)

minSymm :: SNat m -> SNat n -> Dict (Min m n ~ Min n m)
minSymm m n = alternative (leqGeqDec m n)
    (using (geqMin n m) Dict)
    (using (geqMin m n) Dict)
