{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data.Heap
    ( Heap'(..) 
    , empty
    , merge
    , singleton
    , insert
    ) where

import Data.Nat

infix 4 <=??

type family (m :: Nat) <=?? (n :: Maybe Nat) :: Bool where
    _ <=?? 'Nothing  = 'True
    m <=?? ('Just n) = m <=? n

data Heap' :: Maybe Nat -> Nat -> * -> * where

    Empty :: Heap' 'Nothing 'Z a

    Tree :: ( ((p <=?? p') ~ 'True)
            , ((p <=?? p'') ~ 'True)
            , ((r'' <=? r') ~ 'True) 
            )
            => SNat p
            -> SNat ('S r'')
            -> a
            -> Heap' p' r' a
            -> Heap' p'' r'' a
            -> Heap' ('Just p) ('S r'') a

deriving instance Show a => Show (Heap' p r a)

deriving instance Functor (Heap' p r)

data Heap :: * -> * where

    Heap :: Heap' p r a -> Heap a

deriving instance Show a => Show (Heap a)

deriving instance Functor Heap

empty :: Heap a
empty = Heap Empty

merge :: Heap a -> Heap a -> Heap a
merge (Heap h) (Heap h') = case (h, h') of
    (Empty           , y                    ) -> Heap y
    (y               , Empty                ) -> Heap y
    (Tree p r x ys zs, Tree p' r' x' ys' zs') -> undefined

singleton :: SNat n -> a -> Heap a
singleton p x = Heap $ Tree p (SS SZ) x Empty Empty

insert :: SNat n -> a -> Heap a -> Heap a
insert p x = merge (singleton p x)
