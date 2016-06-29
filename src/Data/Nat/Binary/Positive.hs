{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Nat.Binary.Positive
    ( PNat(..)
    , type (??)
    , type (<)
    , type (<=)
    , type (>=)
    , type (>)
    , type Min
    , SPNat(..)
    , SPNAT(..)
    , toSPNAT
    , toNatural
    , eqSame
    , sameEq
    , decPNat
    ) where

import Data.Constraint
import Data.Logic
import Numeric.Natural

data PNat = One | Even PNat | Odd PNat deriving (Show, Read, Eq)

infix 4 ??

type family EO (o :: Ordering) :: Ordering where
    EO 'LT = 'LT
    EO 'EQ = 'LT
    EO 'GT = 'GT

type family OE (o :: Ordering) :: Ordering where
    OE 'LT = 'LT
    OE 'EQ = 'GT
    OE 'GT = 'GT

type family (m :: PNat) ?? (n :: PNat) :: Ordering where
    'One    ?? 'One    = 'EQ
    'One    ?? _       = 'LT
    'Even _ ?? 'One    = 'GT
    'Even m ?? 'Even n = m ?? n
    'Even m ?? 'Odd n  = EO (m ?? n)
    'Odd _  ?? 'One    = 'GT
    'Odd m  ?? 'Even n = OE (m ?? n)
    'Odd m  ?? 'Odd n  = m ?? n

infix 4 <, <=, >=, >

type family IsLeq (o :: Ordering) :: Bool where
    IsLeq 'LT = 'True
    IsLeq 'EQ = 'True
    IsLeq 'GT = 'False

type (m :: PNat) < (n :: PNat) = (m ?? n) ~ 'LT

type (m :: PNat) <= (n :: PNat) = IsLeq (m ?? n) ~ 'True

type (m :: PNat) >= (n :: PNat) = n <= m

type (m :: PNat) > (n :: PNat) = n < m

type Min (m :: PNat) (n :: PNat) = IfThenElse (IsLeq (m ?? n)) m n

data SPNat :: PNat -> * where

    SOne :: SPNat 'One

    SEven :: SPNat n -> SPNat ('Even n)

    SOdd :: SPNat n -> SPNat ('Odd n)

data SPNAT :: * where

    SPNAT :: SPNat n -> SPNAT

toSPNAT :: Natural -> Maybe SPNAT
toSPNAT 0 = Nothing
toSPNAT 1 = Just $ SPNAT SOne
toSPNAT n = case toSPNAT (n `div` 2) of
    Just (SPNAT n') | even n    -> Just $ SPNAT $ SEven n'
                    | otherwise -> Just $ SPNAT $ SOdd n'
    Nothing                     -> error "impossible branch"

toNatural :: SPNat n -> Natural
toNatural SOne      = 1
toNatural (SEven n) = 2 * toNatural n
toNatural (SOdd n)  = 1 + 2 * toNatural n

instance Show (SPNat n) where

    show = show . toNatural

instance Show SPNAT where

    show (SPNAT n) = show n

data Dec (m :: PNat) (n :: PNat) = DecLT (Dict (m < n)) | DecEQ (Dict (m ~ n)) | DecGT (Dict (m > n))
    deriving Show

eqSame :: (m ?? n) ~ 'EQ => SPNat m -> SPNat n -> Dict (m ~ n)
eqSame SOne      SOne      = Dict
eqSame (SEven m) (SEven n) = using (eqSame m n) Dict
eqSame (SEven m) (SOdd n)  = case decPNat m n of
eqSame (SOdd m)  (SEven n) = case decPNat m n of
eqSame (SOdd m)  (SOdd n)  = using (eqSame m n) Dict

sameEq :: SPNat n -> Dict ((n ?? n) ~ 'EQ)
sameEq SOne      = Dict 
sameEq (SEven n) = using (sameEq n) Dict
sameEq (SOdd n)  = using (sameEq n) Dict

decPNat :: SPNat m -> SPNat n -> Dec m n
decPNat SOne      SOne      = DecEQ Dict
decPNat SOne      (SEven _) = DecLT Dict
decPNat SOne      (SOdd _)  = DecLT Dict
decPNat (SEven _) SOne      = DecGT Dict
decPNat (SEven m) (SEven n) = case decPNat m n of
    DecLT Dict -> DecLT Dict
    DecEQ Dict -> DecEQ Dict
    DecGT Dict -> DecGT Dict
decPNat (SEven m) (SOdd n) = case decPNat m n of
    DecLT Dict -> DecLT Dict
    DecEQ Dict -> using (sameEq m) $ DecLT Dict
    DecGT Dict -> DecGT Dict
decPNat (SOdd _)  SOne      = DecGT Dict
decPNat (SOdd m) (SEven n) = case decPNat m n of
    DecLT Dict -> DecLT Dict
    DecEQ Dict -> using (sameEq m) $ DecGT Dict
    DecGT Dict -> DecGT Dict
decPNat (SOdd m) (SOdd n) = case decPNat m n of
    DecLT Dict -> DecLT Dict
    DecEQ Dict -> DecEQ Dict
    DecGT Dict -> DecGT Dict

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
