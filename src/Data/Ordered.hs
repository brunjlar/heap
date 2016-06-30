{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Ordered
    ( Ordered(..) 
    , Nat(..)
    , SING(..)
    , Dec(..)
    , type IsLeq
    , type InvOrd
    , type (<), type (<=), type (>=), type (>)
    , type Min
    , sameEq
    , leqGeqEq
    , leqGtDec
    , leqGeqDec
    , geqMin
    , minProd
    , minSymm
    ) where

import Data.Constraint
import Data.Kind 
import Data.Logic
import Numeric.Natural

infix 4 ??, <, <=, >, >=

data SING :: * -> * where

    SING :: Sing a n -> SING a

data Dec (m :: a) (n :: a) where

    DecLT :: Dict (m < n) -> Dec m n 

    DecEQ :: Dict ((m ?? n) ~ 'EQ) -> Dec m n 

    DecGT :: Dict (m > n) -> Dec m n 

type family InvOrd (o :: Ordering) :: Ordering where

    InvOrd 'LT = 'GT
    InvOrd 'EQ = 'EQ
    InvOrd 'GT = 'LT

class Ordered a where

    type (??) (m :: a) (n :: a) :: Ordering
    
    data Sing a :: a -> *

    symm :: Sing a m -> Sing a n -> Dict ((n ?? m) ~ InvOrd (m ?? n))

    eqSame :: (m ?? n) ~ 'EQ => Sing a m -> Sing a n -> Dict (m ~ n)

    dec :: Sing a m -> Sing a n -> Dec m n

class Ordered nat => Nat nat where

    type Zero nat :: nat

    type Succ nat (n :: nat) :: nat

    zero :: Sing nat (Zero nat)

    succ' :: Sing nat n -> Sing nat (Succ nat n)

    toSING :: Natural -> SING nat

    toNatural :: Sing nat n -> Natural

instance Nat nat => Show (Sing nat n) where

    show = show . toNatural

instance Nat nat => Show (SING nat) where

    show (SING n) = show n

type family IsLeq (o :: Ordering) :: Bool where
    IsLeq 'LT = 'True
    IsLeq 'EQ = 'True
    IsLeq 'GT = 'False

type (m :: a) < (n :: a) = (m ?? n) ~ 'LT

type (m :: a) <= (n :: a) = IsLeq (m ?? n) ~ 'True

type (m :: a) >= (n :: a) = n <= m

type (m :: a) > (n :: a) = n < m

sameEq :: Ordered a => Sing a n -> Dict ((n ?? n) ~ 'EQ)
sameEq n = using (symm n n) $ case dec n n of
    DecEQ d -> d
    _       -> error "impossible branch"

type Min (m :: a) (n :: a) = IfThenElse (IsLeq (m ?? n)) m n

leqGeqEq :: (Ordered a, m <= n, m >= n) => Sing a m -> Sing a n -> Dict (m ~ n)
leqGeqEq m n = case dec m n of
    DecEQ Dict -> using (eqSame m n) Dict
    _          -> error "impossible branch"

leqGtDec :: Ordered a => Sing a m -> Sing a n -> Either (Dict (m <= n)) (Dict (m > n))
leqGtDec m n = case dec m n of
    DecLT Dict -> Left Dict
    DecEQ Dict -> using (sameEq m) $ Left Dict
    DecGT Dict -> Right Dict

leqGeqDec :: Ordered a => Sing a m -> Sing a n -> Either (Dict (m <= n)) (Dict (m >= n))
leqGeqDec m n = alternative (leqGtDec m n) (Left Dict) (Right Dict)

geqMin :: (Ordered a, m >= n) => Sing a m -> Sing a n -> Dict (Min m n ~ n)
geqMin m n = case dec m n of
    DecEQ Dict -> using (eqSame m n) Dict
    DecGT Dict -> using (symm n m) Dict
    _          -> error "impossible branch"

minProd :: (Ordered a, l <= m, l <= n) 
           => Sing a l -> Sing a m -> Sing a n -> Dict (l <= Min m n)
minProd _ m n = alternative (leqGeqDec m n)
    Dict
    (using (geqMin m n) Dict)

minSymm :: Ordered a => Sing a m -> Sing a n -> Dict (Min m n ~ Min n m)
minSymm m n = alternative (leqGeqDec m n)
    (using (geqMin n m) Dict)
    (using (geqMin m n) Dict)
