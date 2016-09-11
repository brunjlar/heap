{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Data.Logic
Description : logic utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

Utilities for doing logic and proofs in Haskell.
-}

module Data.Logic
    ( combine
    , using
    , alternative
    , type IfThenElse
    ) where

import Data.Constraint

-- | Combines two reified facts into one.
--
combine :: Dict a -> Dict b -> Dict (a, b)
combine Dict Dict = Dict

-- | /Modus ponens/ for reified facts: If @a@ is known, and if @a@ implies @b@, then @b@ is known.
--
using :: Dict a -> (a => b) -> b
using d x = case d of Dict -> x
{-# INLINE using #-}

-- | Elimination of /or/: If @a@ or @b@ are known, and if both @a@ and @b@ imply @c@,
--   then @c@ is known.
--
alternative :: Either (Dict a) (Dict b) -> (a => c) -> (b => c) -> c
alternative (Left  Dict) x _ = x
alternative (Right Dict) _ y = y
{-# INLINE alternative #-}

-- | If-then-else on the type level, in the form of a type family.
--
type family IfThenElse (a :: Bool) (b :: k) (c :: k) :: k where
    IfThenElse 'True  b _ = b
    IfThenElse 'False _ c = c
