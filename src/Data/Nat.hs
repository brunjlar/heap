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
    , leqDec
    , leqMin
    , minSymm
    ) where

import Data.Constraint
import Numeric.Natural

data Nat = Z | S Nat deriving (Show, Read, Eq)

infix 4 <=, <=?

type family (m :: Nat) <=? (n :: Nat) :: Bool where
    'Z   <=? _    = 'True
    'S _ <=? 'Z   = 'False
    'S m <=? 'S n = m <=? n

type (m :: Nat) <= (n :: Nat) = (m <=? n) ~ 'True

type family Min (m :: Nat) (n :: Nat) :: Nat where
    Min 'Z     _      = 'Z
    Min _      'Z     = 'Z
    Min ('S m) ('S n) = 'S (Min m n)

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

leqDec :: SNat m -> SNat n -> Either (Dict (m <= n)) (Dict (n <= m))
leqDec SZ     _      = Left Dict
leqDec _      SZ     = Right Dict
leqDec (SS m) (SS n) = leqDec m n

leqMin :: (m <= n) => SNat m -> SNat n -> Dict (Min m n ~ m)
leqMin SZ _ = Dict
leqMin (SS m) (SS n) = case leqMin m n of Dict -> Dict

minSymm :: SNat m -> SNat n -> Dict (Min m n ~ Min n m)
minSymm SZ     _      = Dict
minSymm _      SZ     = Dict
minSymm (SS m) (SS n) = case minSymm m n of Dict -> Dict
