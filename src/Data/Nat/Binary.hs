{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Nat.Binary
    ( PNat(..)
    , type (???)
    , Nat(..)
    , type (??)
    , type (<)
    , type (<=)
    , type (>=)
    , type (>)
    , type Min
    , SPNat(..)
    , SNat(..)
    , SNAT(..)
    , toSNAT
    , toNaturalP
    , toNatural
    ) where

import Data.Constraint
import Data.Logic
import Numeric.Natural

data PNat = One | Even PNat | Odd PNat deriving (Show, Read, Eq)

infix 4 ???

type family EO (o :: Ordering) :: Ordering where
    EO 'LT = 'LT
    EO 'EQ = 'LT
    EO 'GT = 'GT

type family OE (o :: Ordering) :: Ordering where
    OE 'LT = 'LT
    OE 'EQ = 'GT
    OE 'GT = 'GT

type family (m :: PNat) ??? (n :: PNat) :: Ordering where
    'One    ??? 'One    = 'EQ
    'One    ??? _       = 'LT
    'Even _ ??? 'One    = 'GT
    'Even m ??? 'Even n = m ??? n
    'Even m ??? 'Odd n  = EO (m ??? n)
    'Odd _  ??? 'One    = 'GT
    'Odd m  ??? 'Even n = OE (m ??? n)
    'Odd m  ??? 'Odd n  = m ??? n

data Nat = Zero | PNat PNat deriving (Show, Read, Eq)

infix 4 <, <=, >=, >, ??

type family (m :: Nat) ?? (n :: Nat) :: Ordering where
    'Zero   ?? 'Zero   = 'EQ
    'Zero   ?? 'PNat _ = 'LT
    'PNat _ ?? 'Zero   = 'GT
    'PNat m ?? 'PNat n = m ??? n

type family IsLeq (o :: Ordering) :: Bool where
    IsLeq 'LT = 'True
    IsLeq 'EQ = 'True
    IsLeq 'GT = 'False

type (m :: Nat) < (n :: Nat) = (m ?? n) ~ 'LT

type (m :: Nat) <= (n :: Nat) = IsLeq (m ?? n) ~ 'True

type (m :: Nat) >= (n :: Nat) = n <= m

type (m :: Nat) > (n :: Nat) = n < m

type Min (m :: Nat) (n :: Nat) = IfThenElse (IsLeq (m ?? n)) m n

data SPNat :: PNat -> * where

    SOne :: SPNat 'One

    SEven :: SPNat n -> SPNat ('Even n)

    SOdd :: SPNat n -> SPNat ('Odd n)

data SNat :: Nat -> * where

    SZero :: SNat 'Zero

    SPNat :: SPNat n -> SNat ('PNat n)

data SNAT :: * where

    SNAT :: SNat n -> SNAT

toSNAT :: Natural -> SNAT
toSNAT 0 = SNAT SZero
toSNAT 1 = SNAT $ SPNat SOne
toSNAT n = case toSNAT (n `div` 2) of
    SNAT (SPNat n') | even n    -> SNAT $ SPNat $ SEven n'
                    | otherwise -> SNAT $ SPNat $ SOdd n'
    SNAT SZero                  -> error "impossible branch"

toNaturalP :: SPNat n -> Natural
toNaturalP SOne      = 1
toNaturalP (SEven n) = 2 * toNaturalP n
toNaturalP (SOdd n)  = 1 + 2 * toNaturalP n

toNatural :: SNat n -> Natural
toNatural SZero = 0
toNatural (SPNat n) = toNaturalP n

instance Show (SPNat n) where

    show = show . toNaturalP

instance Show (SNat n) where

    show = show . toNatural

instance Show SNAT where

    show (SNAT n) = show n

{-

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
    -}
