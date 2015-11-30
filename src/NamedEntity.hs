{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module NamedEntity where

{- Our aid is
type Person = "person" ::: "id"        :> Int
                        +> "firstname" :> Text
                        +> "lastname"  :> Text
                        +> "birthday"  :> Day
-}

import Data.Typeable(Proxy(..), Typeable(..))
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

infixr 9 :>
newtype s :> val = V val deriving (Typeable, Show, Eq, Ord, Functor)

class Named a where
    type TName a :: Symbol

instance Named ((n::Symbol):>v) where
    type TName (n:>v) = n

type family Div (k::Nat) (n::Nat) where
    Div k n = Div' k n (CmpNat k n)

type family Div' (k::Nat) (n::Nat) (ord::Ordering) where
    Div' k n LT = 0
    Div' k n EQ = 1
    Div' k n GT = 1 + Div (k-n) n

type family Mod (k::Nat) (n::Nat) where
    Mod k n = k - n * Div k n

type family Tu4 t where
    Tu4 (1:>a)          = a
    Tu4 (2:>a:>b)       = (a,b)
    Tu4 (3:>a:>b:>c)    = (a,b,c)
    Tu4 (4:>a:>b:>c:>d) = (a,b,c,d)
    Tu4 ((k::Nat):>a)   =

