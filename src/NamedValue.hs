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

infixr 9 :>

newtype (s::Symbol) :> val = V val
    deriving (Typeable, Show, Eq, Ord, Functor, Traversable, Foldable, Monoid, Default)

--instance (Default val) => Default (s:>val) where
--    def = V def
