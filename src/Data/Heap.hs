{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Heap
    ( module Data.Ordered
    , module Numeric.Natural
    , Heap
    , singleton
    , insert
    , toHeap
    , pop
    , peek
    , (<>)
    ) where

import Data.Constraint
import Data.Kind
import Data.Logic
import Data.Monoid     ((<>))
import Data.Ordered
import Numeric.Natural

data Sing' nat :: Maybe nat -> * where

    Nothing' :: Sing' nat 'Nothing

    Just' :: Sing nat n -> Sing' nat ('Just n)

infix 4 <=?, <=.

type family (m :: nat) <=? (n :: Maybe nat) :: Bool where
    _ <=? 'Nothing = 'True
    m <=? 'Just n  = IsLeq (m ?? n)

type (m :: nat) <=. (n :: Maybe nat) = (m <=? n) ~ 'True

type family Min' (m :: Maybe nat) (n :: Maybe nat) :: Maybe nat where
    Min' 'Nothing  n         = n
    Min' m         'Nothing  = m
    Min' ('Just m) ('Just n) = 'Just (Min m n)

minProd' :: (Ordered nat, l <=. m, l <=. n) => Sing nat l -> Sing' nat m -> Sing' nat n -> Dict (l <=. Min' m n)
minProd' _ Nothing'  Nothing'  = Dict
minProd' _ (Just' _) Nothing'  = Dict
minProd' _ Nothing'  (Just' _) = Dict
minProd' l (Just' m) (Just' n) = using (minProd l m n) Dict

data Heap' nat (p :: Maybe nat) (r :: nat) a where

    Empty :: Heap' nat 'Nothing (Zero nat) a

    Tree :: ( (p   <=. p')
            , (p   <=. p'')
            , (r'' <=  r')
            )
            => !(Sing nat p)
            -> !(Sing nat (Succ nat r''))
            -> !a
            -> !(Heap' nat p' r' a)
            -> !(Heap' nat p'' r'' a)
            -> Heap' nat ('Just p) (Succ nat r'') a

deriving instance (Nat nat, Show a) => Show (Heap' nat p r a)

deriving instance Functor (Heap' nat p r)

rank :: Nat nat => Heap' nat p r a -> Sing nat r
rank Empty            = zero
rank (Tree _ r _ _ _) = r

priority :: Heap' nat p r a -> Sing' nat p
priority Empty            = Nothing'
priority (Tree p _ _ _ _) = Just' p

data Heap'' nat (p :: Maybe nat) a where

    Heap'' :: Heap' nat p r a -> Heap'' nat p a

deriving instance (Nat nat, Show a) => Show (Heap'' nat p a)

deriving instance Functor (Heap'' nat p)

data Heap nat a where

    Heap :: Heap'' nat p a -> Heap nat a

deriving instance (Nat nat, Show a) => Show (Heap nat a)

deriving instance Functor (Heap nat)

singleton :: forall nat a. Nat nat => Natural -> a -> Heap nat a
singleton p x = case toSING @nat p of SING p' -> let z = zero @nat
                                                 in  using (sameEq z) $ Heap $ Heap'' $ Tree p' (succ' z) x Empty Empty
{-# INLINE singleton #-}

merge :: Nat nat => Heap'' nat p a -> Heap'' nat q a -> Heap'' nat (Min' p q) a
merge (Heap'' Empty)                h'                           = h'
merge h                             (Heap'' Empty)               = h
merge h@(Heap'' (Tree p _ x ys zs)) h'@(Heap'' (Tree q _ _ _ _)) =
    alternative (ltGeqDec q p)
        (using (minSymm p q) $ merge h' h) $
        let h'' = merge (Heap'' zs) h'
        in  case h'' of
            Heap'' Empty                 -> error "impossible branch"
            Heap'' h'''@(Tree _ r _ _ _) ->
                using (minProd' p (priority zs) (Just' q)) $
                    alternative (leqGtDec r $ rank ys)
                        (Heap'' $ Tree p (succ' r) x ys h''')
                        (Heap'' $ Tree p (succ' $ rank ys) x h''' ys)

instance Nat nat => Monoid (Heap nat a) where

    mempty = Heap $ Heap'' Empty

    Heap h `mappend` Heap h' = Heap $ merge h h'

toHeap :: (Nat nat, Foldable f) => f (Natural, a) -> Heap nat a
toHeap = foldMap (uncurry singleton)

insert :: Nat nat => Natural -> a -> Heap nat a -> Heap nat a
insert p = mappend . singleton p
{-# INLINE insert #-}

pop :: Nat nat => Heap nat a -> Maybe (Natural, a, Heap nat a)
pop (Heap (Heap'' Empty))              = Nothing
pop (Heap (Heap'' (Tree p _ x ys zs))) = Just (toNatural p, x, Heap (Heap'' ys) <> Heap (Heap'' zs))
{-# INLINE pop #-}

peek :: Nat nat => Heap nat a -> Maybe (Natural, a)
peek h = pop h >>= \(p, x, _) -> return (p, x)
{-# INLINE peek #-}

instance Nat nat => Foldable (Heap nat) where

    foldMap f = go mempty where

        go m h = case pop h of
            Nothing         -> m
            Just (_, x, h') -> go (m <> f x) h'
