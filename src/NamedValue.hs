{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedValue where
import Data.Default(Default(..))
import Data.Typeable(Typeable(..))
import GHC.TypeLits(Symbol, symbolVal, KnownSymbol)
import Lens.Micro(Lens', lens)
import Data.Proxy

infixr 9 :>
-- infixr 9 :!>

-- | "Named Value" or "field". It has field name and field value.
--   There is no runtime penalty as it is just a newtype with deriving instances
newtype (s::Symbol) :> val = V val
    deriving (Typeable, Show, Eq, Ord, Functor, Traversable, Foldable, Monoid, Default)

-- Lens for convenient composition
valLens :: Lens' (n:>v) v
valLens = lens (\(V val) -> val) (\(V _) val -> V val)

getName :: (KnownSymbol n) => (n:>v) -> String
getName (_ :: n:>v) = symbolVal (Proxy :: Proxy n)
-- type (s::ks) ::> (v::kv) = s

{-
data (p::kp) ::> (v::kv) = Prop
instance Default (p ::> v) where
    def = Prop
-}
{-
newtype (s::Symbol) :!> val = K val
    deriving (Typeable, Show, Eq, Ord, Functor, Traversable, Foldable, Monoid, Default)

type family Field a where
    Field (s :> v) = s :> v
    Field (s :!> v) = s :!> v
-}
