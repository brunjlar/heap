{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Nat
    ( Nat(..)
    , type (??)
    , type (<=?)
    , SNat(..)
    , SNAT
    , toNatural
    , toSNAT
    , decNat
    , decNat'
    ) where

import Numeric.Natural

data Nat = Z | S Nat deriving (Show, Read, Eq)

infix 4 ??, <=?

type family (m :: Nat) ?? (n :: Nat) :: Ordering where
    'Z   ?? 'Z   = 'EQ
    'Z   ?? _    = 'LT
    _    ?? 'Z   = 'GT
    'S m ?? 'S n = m ?? n

type family (m :: Nat) <=? (n :: Nat) :: Bool where
    'Z   <=? _    = 'True
    'S _ <=? 'Z   = 'False
    'S m <=? 'S n = m <=? n

data SNat :: Nat -> * where

    SZ :: SNat 'Z

    SS :: SNat n -> SNat ('S n)

data SNAT :: * where

    SNAT :: SNat n -> SNAT

toSNAT :: Natural -> SNAT
toSNAT 0 = SNAT SZ
toSNAT n = case toSNAT (pred n) of
    SNAT n' -> SNAT (SS n')

toNatural :: SNat n -> Natural
toNatural SZ     = 0
toNatural (SS n) = succ $ toNatural n

instance Show (SNat n) where

    show = show . toNatural

instance Show SNAT where

    show (SNAT n) = show n

decNat ::    SNat m
          -> SNat n
          -> (((m ?? n) ~ 'LT) => a)
          -> (((m ?? n) ~ 'EQ) => a)
          -> (((m ?? n) ~ 'GT) => a)
          -> a
decNat SZ     SZ     _ a _ = a
decNat SZ     (SS _) a _ _ = a
decNat (SS _) SZ     _ _ a = a
decNat (SS m) (SS n) a b c = decNat m n a b c

decNat' ::    SNat m
           -> SNat n
           -> (((m <=? n) ~ 'True) => a)
           -> (((n <=? m) ~ 'True) => a)
           -> a
decNat' SZ     _      a _ = a
decNat' (SS _) SZ     _ a = a
decNat' (SS m) (SS n) a b = decNat' m n a b
