{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-|
Module      : Data.Ordered
Description : type classes for natural numbers
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

Defines type classes for types of natural numbers.
-}

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
    , ltGeqDec
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

-- | Singletons that have "forgotten" their type annotation.
--
data SING :: * -> * where

    SING :: Sing a n -> SING a

-- | Decidable totality of an order relation on the type level.
--
data Dec (m :: a) (n :: a) where

    DecLT :: Dict (m < n) -> Dec m n

    DecEQ :: Dict ((m ?? n) ~ 'EQ) -> Dec m n

    DecGT :: Dict (m > n) -> Dec m n

-- | The inverse on 'Ordering' on the type level.
--
type family InvOrd (o :: Ordering) :: Ordering where

    InvOrd 'LT = 'GT
    InvOrd 'EQ = 'EQ
    InvOrd 'GT = 'LT

-- | Type class for types which are (totally) ordered on the type level.
--
class Ordered a where

    -- | Type level ordering relation.
    type (??) (m :: a) (n :: a) :: Ordering

    -- | Associated singleton type.
    data Sing a :: a -> *

    -- | Symmetry of the order relation.
    symm :: Sing a m -> Sing a n -> Dict ((n ?? m) ~ InvOrd (m ?? n))

    -- | Compatibility of order and type-equality.
    eqSame :: (m ?? n) ~ 'EQ => Sing a m -> Sing a n -> Dict (m ~ n)

    -- | Totality of the order relation.
    dec :: Sing a m -> Sing a n -> Dec m n

-- | Type class for types of natural numbers which are totally ordered on the type level.
--
class Ordered nat => Nat nat where

    -- | Type level zero.
    type Zero nat :: nat

    -- | Type level successor function.
    type Succ nat (n :: nat) :: nat

    -- | Singleton zero.
    zero :: Sing nat (Zero nat)

    -- | Singleton successor function.
    succ' :: Sing nat n -> Sing nat (Succ nat n)

    -- | Converts a 'Natural' to a singleton.
    toSING :: Natural -> SING nat

    -- | Converts a singleton to a 'Natural'.
    toNatural :: Sing nat n -> Natural

instance Nat nat => Show (Sing nat n) where

    show = show . toNatural

instance Nat nat => Show (SING nat) where

    show (SING n) = show n

-- | Auxiliary type level function to define "is less than or equal".
--
type family IsLeq (o :: Ordering) :: Bool where
    IsLeq 'LT = 'True
    IsLeq 'EQ = 'True
    IsLeq 'GT = 'False

-- | "Less than" on the type level.
--
type (m :: a) < (n :: a) = (m ?? n) ~ 'LT

-- | "Less than or equal" on the type level.
--
type (m :: a) <= (n :: a) = IsLeq (m ?? n) ~ 'True

-- | "Greater than or equal" on the type level.
--
type (m :: a) >= (n :: a) = n <= m

-- | "Greater than" on the type level.
--
type (m :: a) > (n :: a) = n < m

-- | Statement of the fact that for any instance of 'Ordered',
-- | each element is equal to itself.
--
sameEq :: Ordered a => Sing a n -> Dict ((n ?? n) ~ 'EQ)
sameEq n = using (symm n n) $ case dec n n of
    DecEQ d -> d
    _       -> error "impossible branch"
{-# INLINE sameEq #-}

-- | The function 'min' on the type level.
--
type Min (m :: a) (n :: a) = IfThenElse (IsLeq (m ?? n)) m n

-- | Statement of the fact that the ordering relation for an instance of class 'Ordered' is antisymmetric,
--   i.e. @m <= n@ and @m >= n@ imply @m == n@.
--
leqGeqEq :: (Ordered a, m <= n, m >= n) => Sing a m -> Sing a n -> Dict (m ~ n)
leqGeqEq m n = case dec m n of
    DecEQ Dict -> using (eqSame m n) Dict
    _          -> error "impossible branch"
{-# INLINE leqGeqEq #-}

-- | Statement of the fact that for an instance of 'Ordered', for any two elements @m@ and @n@,
--   either @m <= n@ or @m > n@ holds.
--
leqGtDec :: Ordered a => Sing a m -> Sing a n -> Either (Dict (m <= n)) (Dict (m > n))
leqGtDec m n = case dec m n of
    DecLT Dict -> Left Dict
    DecEQ Dict -> using (sameEq m) $ Left Dict
    DecGT Dict -> Right Dict
{-# INLINE leqGtDec #-}

-- | Statement of the fact that for an instance of 'Ordered', for any two elements @m@ and @n@,
--   either @m < n@ or @m >= n@ holds.
--
ltGeqDec :: Ordered a => Sing a m -> Sing a n -> Either (Dict (m < n)) (Dict (m >= n))
ltGeqDec m n = case leqGtDec n m of
    Left Dict  -> Right Dict
    Right Dict -> Left Dict
{-# INLINE ltGeqDec #-}

-- | Statement of the fact that for an instance of 'Ordered', for any two elements @m@ and @n@,
--   @m <= n@ or @m >= n@ holds.
--
leqGeqDec :: Ordered a => Sing a m -> Sing a n -> Either (Dict (m <= n)) (Dict (m >= n))
leqGeqDec m n = alternative (leqGtDec m n) (Left Dict) (Right Dict)
{-# INLINE leqGeqDec #-}

-- | Statement of the fact that for an instance of 'Ordered', for any two elements @m@ and @n@,
--   @m >= n@ implies that @n@ is the minimum of @m@ and @n@.
--
geqMin :: (Ordered a, m >= n) => Sing a m -> Sing a n -> Dict (Min m n ~ n)
geqMin m n = case dec m n of
    DecEQ Dict -> using (eqSame m n) Dict
    DecGT Dict -> using (symm n m) Dict
    _          -> error "impossible branch"
{-# INLINE geqMin #-}

-- | Statement of the fact that for an instance of 'Ordered', for any three elements @l@, @m@ and @n@,
--   @l <= m@ and @l <= n@ imply @l <= min m n@.
--
minProd :: (Ordered a, l <= m, l <= n)
           => Sing a l -> Sing a m -> Sing a n -> Dict (l <= Min m n)
minProd _ m n = alternative (leqGeqDec m n)
    Dict
    (using (geqMin m n) Dict)
{-# INLINE minProd #-}

-- | Statement of the fact that for an instance of 'Ordered', for any two elements @m@ and @n@,
--   the minimum of @m@ and @n@ is the same as the minimum of @n@ and @m@.
--
minSymm :: Ordered a => Sing a m -> Sing a n -> Dict (Min m n ~ Min n m)
minSymm m n = alternative (leqGeqDec m n)
    (using (geqMin n m) Dict)
    (using (geqMin m n) Dict)
{-# INLINE minSymm #-}
