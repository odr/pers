{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module NamedValue where
import Data.Default(Default(..))
import Data.Typeable(Typeable(..))
import GHC.TypeLits(Symbol)

infixr 9 :>

newtype (s::Symbol) :> val = V val
    deriving (Typeable, Show, Eq, Ord, Functor, Traversable, Foldable)

instance (Default val) => Default (s:>val) where
    def = V def
