{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}

module Data.Heap
    ( Heap
    , singleton
    , insert
    , toHeap
    , pop
    , peek
    ) where

import Data.Constraint
import Data.Monoid     ((<>))
import Data.Nat
import Numeric.Natural

c :: Dict a -> Dict b -> Dict (a, b)
c Dict Dict = Dict

data SNat' :: Maybe Nat -> * where

    Nothing' :: SNat' 'Nothing

    Just' :: SNat n -> SNat' ('Just n)

infix 4 <=??, <=.

type family (m :: Nat) <=?? (n :: Maybe Nat) :: Bool where
    _ <=?? 'Nothing  = 'True
    m <=?? ('Just n) = m <=? n

type (m :: Nat) <=. (n :: Maybe Nat) = (m <=?? n) ~ 'True 

type family Min' (m :: Maybe Nat) (n :: Maybe Nat) :: Maybe Nat where
    Min' 'Nothing  n         = n
    Min' m         'Nothing  = m
    Min' ('Just m) ('Just n) = 'Just (Min m n)

leqMin'' :: (l <=. m, l <=. n) => SNat l -> SNat' m -> SNat' n -> Dict (l <=. Min' m n)
leqMin'' SZ     Nothing'       Nothing'       = Dict
leqMin'' SZ     Nothing'       (Just' _)      = Dict
leqMin'' SZ     (Just' _)      Nothing'       = Dict
leqMin'' SZ     (Just' _)      (Just' _)      = Dict
leqMin'' (SS _) Nothing'       Nothing'       = Dict
leqMin'' (SS _) Nothing'       (Just' _)      = Dict
leqMin'' (SS _) (Just' _)      Nothing'       = Dict
leqMin'' (SS l) (Just' (SS m)) (Just' (SS n)) = case leqMin'' l (Just' m) (Just' n) of Dict -> Dict

data Heap' :: Maybe Nat -> Nat -> * -> * where

    Empty :: Heap' 'Nothing 'Z a

    Tree :: ( (p   <=. p')
            , (p   <=. p'')
            , (r'' <=  r') 
            )
            => SNat p
            -> SNat ('S r'')
            -> a
            -> Heap' p' r' a
            -> Heap' p'' r'' a
            -> Heap' ('Just p) ('S r'') a

deriving instance Show a => Show (Heap' p r a)

deriving instance Functor (Heap' p r)

rank :: Heap' p r a -> SNat r
rank Empty            = SZ
rank (Tree _ r _ _ _) = r

priority :: Heap' p r a -> SNat' p
priority Empty            = Nothing'
priority (Tree p _ _ _ _) = Just' p

data Heap'' :: Maybe Nat -> * -> * where

    Heap'' :: Heap' p r a -> Heap'' p a

deriving instance Show a => Show (Heap'' p a)

deriving instance Functor (Heap'' p)

data Heap :: * -> * where

    Heap :: Heap'' p a -> Heap a

deriving instance Show a => Show (Heap a)

deriving instance Functor Heap

singleton :: Natural -> a -> Heap a
singleton p x = case toSNAT p of SNAT p' -> Heap $ Heap'' $ Tree p' (SS SZ) x Empty Empty

merge :: Heap'' p a -> Heap'' q a -> Heap'' (Min' p q) a
merge (Heap'' Empty)                h'                           = h'
merge h                             (Heap'' Empty)               = h
merge h@(Heap'' (Tree p _ x ys zs)) h'@(Heap'' (Tree q _ _ _ _)) =
    case leqDec p q of
        Right Dict -> case minSymm p q of Dict -> merge h' h 
        Left Dict  -> let h'' = merge (Heap'' zs) h'
                      in  case h'' of
                        Heap'' Empty                 -> error "impossible branch"
                        Heap'' h'''@(Tree _ r _ _ _) ->
                            case (leqMin p q) `c` (leqMin'' p (priority zs) (Just' q)) of
                                Dict -> case leqDec r (rank ys) of
                                    Left Dict -> Heap'' $ Tree p (SS r) x ys h'''
                                    Right Dict -> Heap'' $ Tree p (SS $ rank ys) x h''' ys

instance Monoid (Heap a) where

    mempty = Heap $ Heap'' Empty

    Heap h `mappend` Heap h' = Heap $ merge h h'

toHeap :: Foldable f => f (Natural, a) -> Heap a
toHeap = foldMap (uncurry singleton)

insert :: Natural -> a -> Heap a -> Heap a
insert p = mappend . singleton p

pop :: Heap a -> Maybe (Natural, a, Heap a)
pop (Heap (Heap'' Empty))              = Nothing
pop (Heap (Heap'' (Tree p _ x ys zs))) = Just (toNatural p, x, Heap (Heap'' ys) <> Heap (Heap'' zs)) 

peek :: Heap a -> Maybe (Natural, a)
peek h = pop h >>= \(p, x, _) -> return (p, x)

instance Foldable Heap where

    foldMap f h = go mempty h where

        go m h' = case pop h' of
            Nothing         -> m
            Just (_, x, h'') -> go (m <> f x) h''
