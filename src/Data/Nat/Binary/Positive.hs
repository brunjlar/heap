{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.Nat.Binary.Positive
Description : positive binary natural numbers
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

Defines positive binary natural numbers, i.e. positive natural numbers with binary representation.
They are more efficient than Peano natural numbers, but also more complicated.
-}

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

-- | Positive binary natural numbers. @One = 1@, @Even One = 2@, @Odd One = 3@,
--   @Even Even One = 4@, @Odd Even One = 5@, @Even Odd One = 6@, @Odd Odd One = 7@ and so on.
data Pos =
      One       -- ^ one
    | Even !Pos -- ^ times two
    | Odd !Pos  -- ^ times two plus one
    deriving (Show, Read, Eq)

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

-- | Tries to convert a 'Natural' to a singleton positive binary number. For zero, this results in 'Nothing'.
toSINGP :: Natural -> Maybe (SING Pos)
toSINGP 1 = Just (SING SOne)
toSINGP n = case toSINGP (n `div` 2) of
    Just (SING n') | even n    -> Just $ SING $ SEven n'
                   | otherwise -> Just $ SING $ SOdd n'
    Nothing                    -> error "impossible branch"
{-# INLINE toSINGP #-}

-- | Converts a singleton positive natural number to 'Natural'.
toNaturalP :: Sing Pos n -> Natural
toNaturalP SOne      = 1
toNaturalP (SEven n) = 2 * toNaturalP n
toNaturalP (SOdd n)  = 1 + 2 * toNaturalP n
{-# INLINE toNaturalP #-}

-- | Type level successor function.
type family SP (n :: Pos) :: Pos where
    SP 'One      = 'Even 'One
    SP ('Even n) = 'Odd n
    SP ('Odd n)  = 'Even (SP n)

-- | The singleton number one.
one :: Sing Pos 'One
one = SOne
{-# INLINE one #-}

-- | Successor function for singletons.
succP :: Sing Pos n -> Sing Pos (SP n)
succP SOne      = SEven SOne
succP (SEven n) = SOdd n
succP (SOdd n)  = SEven (succP n)
{-# INLINE succP #-}
