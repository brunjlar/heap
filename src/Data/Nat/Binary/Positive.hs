{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Nat.Binary.Positive
    ( Pos(..)
    , toSINGP
    , toNaturalP
    , type SP
    , one
    , succP
    ) where

import Data.Constraint
import Data.Logic
import Data.Ordered
import Numeric.Natural

data Pos = One | Even !Pos | Odd !Pos deriving (Show, Read, Eq)

type family EO (o :: Ordering) :: Ordering where
    EO 'LT = 'LT
    EO 'EQ = 'LT
    EO 'GT = 'GT

type family OE (o :: Ordering) :: Ordering where
    OE 'LT = 'LT
    OE 'EQ = 'GT
    OE 'GT = 'GT

type family (m :: Pos) ??? (n :: Pos) :: Ordering where
    'One    ??? 'One    = 'EQ
    'One    ??? _       = 'LT
    'Even _ ??? 'One    = 'GT
    'Even m ??? 'Even n = m ??? n
    'Even m ??? 'Odd n  = EO (m ??? n)
    'Odd _  ??? 'One    = 'GT
    'Odd m  ??? 'Even n = OE (m ??? n)
    'Odd m  ??? 'Odd n  = m ??? n

instance Ordered Pos where 

    type m ?? n = m ??? n

    data Sing Pos n where

        SOne :: Sing Pos 'One

        SEven :: Sing Pos n -> Sing Pos ('Even n)

        SOdd :: Sing Pos n -> Sing Pos ('Odd n)

    symm SOne      SOne      = Dict
    symm SOne      (SEven _) = Dict
    symm SOne      (SOdd _)  = Dict
    symm (SEven _) SOne      = Dict
    symm (SEven m) (SEven n) = using (symm m n) Dict
    symm (SEven m) (SOdd n)  = using (symm m n) $ case dec m n of
        DecLT Dict -> Dict
        DecEQ Dict -> using (sameEq m) Dict
        DecGT Dict -> using (symm n m) Dict
    symm (SOdd _)  SOne      = Dict
    symm (SOdd m)  (SEven n) = using (symm m n) $ case dec m n of
        DecLT Dict -> Dict
        DecEQ Dict -> using (sameEq m) Dict
        DecGT Dict -> using (symm n m) Dict
    symm (SOdd m)  (SOdd n)  = using (symm m n) Dict
    {-# INLINE symm #-}

    eqSame SOne      SOne      = Dict
    eqSame (SEven m) (SEven n) = using (eqSame m n) Dict
    eqSame (SOdd m)  (SOdd n)  = using (eqSame m n) Dict
    eqSame _         _         = error "impossible branch"
    {-# INLINE eqSame #-}

    dec SOne      SOne      = DecEQ Dict
    dec SOne      (SEven _) = DecLT Dict
    dec SOne      (SOdd _)  = DecLT Dict
    dec (SEven _) SOne      = DecGT Dict
    dec (SEven m) (SEven n) = case dec m n of
        DecLT Dict -> DecLT Dict
        DecEQ Dict -> DecEQ Dict
        DecGT Dict -> DecGT Dict
    dec (SEven m) (SOdd n) = case dec m n of
        DecLT Dict -> DecLT Dict
        DecEQ Dict -> using (sameEq m) $ DecLT Dict
        DecGT Dict -> DecGT Dict
    dec (SOdd _)  SOne      = DecGT Dict
    dec (SOdd m) (SEven n) = case dec m n of
        DecLT Dict -> DecLT Dict
        DecEQ Dict -> using (symm m n) $ DecGT Dict
        DecGT Dict -> DecGT Dict
    dec (SOdd m) (SOdd n) = case dec m n of
        DecLT Dict -> DecLT Dict
        DecEQ Dict -> DecEQ Dict
        DecGT Dict -> DecGT Dict
    {-# INLINE dec #-}

toSINGP :: Natural -> Maybe (SING Pos)
toSINGP 1 = Just (SING SOne)
toSINGP n = case toSINGP (n `div` 2) of
    Just (SING n') | even n    -> Just $ SING $ SEven n'
                   | otherwise -> Just $ SING $ SOdd n'
    Nothing                    -> error "impossible branch"
{-# INLINE toSINGP #-}

toNaturalP :: Sing Pos n -> Natural
toNaturalP SOne      = 1
toNaturalP (SEven n) = 2 * toNaturalP n
toNaturalP (SOdd n)  = 1 + 2 * toNaturalP n
{-# INLINE toNaturalP #-}

type family SP (n :: Pos) :: Pos where
    SP 'One      = 'Even 'One
    SP ('Even n) = 'Odd n
    SP ('Odd n)  = 'Even (SP n)

one :: Sing Pos 'One
one = SOne
{-# INLINE one #-}

succP :: Sing Pos n -> Sing Pos (SP n)
succP SOne      = SEven SOne
succP (SEven n) = SOdd n
succP (SOdd n)  = SEven (succP n)
{-# INLINE succP #-}
