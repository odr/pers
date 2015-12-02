{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedValue where
import Data.Default(Default(..))
import Data.Typeable(Typeable(..))
import GHC.TypeLits(Symbol)
import Lens.Micro(Lens', lens)

infixr 9 :>

-- | "Named Value" or "field". It has field name and field value.
--   There is no runtime penalty as it is just a newtype with deriving instances
newtype (s::Symbol) :> val = V val
    deriving (Typeable, Show, Eq, Ord, Functor, Traversable, Foldable, Monoid, Default)

-- Lens for convenient composition
valLens :: Lens' (n:>v) v
valLens = lens (\(V val) -> val) (\(V _) val -> V val)

