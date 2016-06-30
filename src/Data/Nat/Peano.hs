{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data.Nat.Peano
    ( Peano(..)
    ) where

import Data.Constraint
import Data.Logic
import Data.Ordered

data Peano = Z | S !Peano deriving (Show, Read, Eq)

infix 4 ???

type family (m :: Peano) ??? (n :: Peano) :: Ordering where
    'Z   ??? 'Z   = 'EQ
    'Z   ??? _    = 'LT
    'S _ ??? 'Z   = 'GT
    'S m ??? 'S n = m ??? n

instance Ordered Peano where

    type m ?? n = m ??? n

    data Sing Peano n where

        SZ :: Sing Peano 'Z

        SS :: Sing Peano n -> Sing Peano ('S n)

    dec SZ     SZ     = DecEQ Dict 
    dec SZ     (SS _) = DecLT Dict
    dec (SS _) SZ     = DecGT Dict 
    dec (SS m) (SS n) = case dec m n of
        DecLT Dict -> DecLT Dict
        DecEQ Dict -> DecEQ Dict
        DecGT Dict -> DecGT Dict
    {-# INLINE dec #-}

    symm SZ     SZ     = Dict
    symm SZ     (SS _) = Dict
    symm (SS _) SZ     = Dict
    symm (SS m) (SS n) = using (symm m n) Dict
    {-# INLINE symm #-}

    eqSame SZ SZ = Dict
    eqSame (SS m) (SS n) = using (eqSame m n) Dict
    {-# INLINE eqSame #-}

instance Nat Peano where

    type Zero Peano = 'Z

    type Succ Peano n = 'S n

    zero = SZ
    {-# INLINE zero #-}

    succ' = SS
    {-# INLINE succ' #-}

    toSING 0 = SING SZ
    toSING n = case toSING (pred n) of
        SING n' -> SING (SS n')
    {-# INLINE toSING #-}

    toNatural SZ     = 0
    toNatural (SS n) = succ $ toNatural n
    {-# INLINE toNatural #-}
