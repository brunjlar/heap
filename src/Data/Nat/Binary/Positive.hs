{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Nat.Binary.Positive
    ( Pos(..)
    ) where

import Data.Constraint
import Data.Logic
import Data.Ordered

data Pos = One | Even Pos | Odd Pos deriving (Show, Read, Eq)

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

    type (??) m n = m ??? n

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

    eqSame SOne      SOne      = Dict
    eqSame (SEven m) (SEven n) = using (eqSame m n) Dict
    eqSame (SEven _) (SOdd _)  = error "impossible branch"
    eqSame (SOdd _)  (SEven _) = error "impossible branch"
    eqSame (SOdd m)  (SOdd n)  = using (eqSame m n) Dict

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

instance Nat Pos where

    toSING 0 = error "zero is not positive"
    toSING 1 = SING SOne
    toSING n = case toSING (n `div` 2) of
        SING n' | even n    -> SING $ SEven n'
                | otherwise -> SING $ SOdd n'

    toNatural SOne      = 1
    toNatural (SEven n) = 2 * toNatural n
    toNatural (SOdd n)  = 1 + 2 * toNatural n
