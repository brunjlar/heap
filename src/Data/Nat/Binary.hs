{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Nat.Binary
    ( Bin(..) 
    ) where

import Data.Constraint
import Data.Logic
import Data.Nat.Binary.Positive
import Data.Ordered

data Bin = Z | P Pos deriving (Show, Read, Eq)

infix 4 ???

type family (m :: Bin) ??? (n :: Bin) :: Ordering where
    'Z   ??? 'Z   = 'EQ
    'Z   ??? 'P _ = 'LT
    'P _ ??? 'Z   = 'GT
    'P m ??? 'P n = m ?? n

instance Ordered Bin where

    type m ?? n = m ??? n

    data Sing Bin n where

        SZ :: Sing Bin 'Z

        SP :: Sing Pos n -> Sing Bin ('P n)

    dec SZ     SZ     = DecEQ Dict
    dec SZ     (SP _) = DecLT Dict
    dec (SP _) SZ     = DecGT Dict
    dec (SP m) (SP n) = case dec m n of
        DecLT Dict -> DecLT Dict
        DecEQ Dict -> DecEQ Dict
        DecGT Dict -> DecGT Dict

    symm SZ     SZ     = Dict
    symm SZ     (SP _) = Dict
    symm (SP _) SZ     = Dict
    symm (SP m) (SP n) = using (symm m n) Dict

    eqSame SZ SZ = Dict
    eqSame (SP m) (SP n) = using (eqSame m n) Dict

type family S (n :: Bin) :: Bin where
    S 'Z     = 'P 'One
    S ('P n) = 'P (SP n)

instance Nat Bin where

    type Zero Bin = 'Z

    type Succ Bin n = S n

    zero = SZ

    succ' SZ     = SP one
    succ' (SP n) = SP (succP n)

    toSING 0 = SING SZ
    toSING n = case toSINGP n of
        Just (SING n') -> SING (SP n')
        Nothing        -> error "impossible branch"

    toNatural SZ     = 0
    toNatural (SP n) = toNaturalP n
