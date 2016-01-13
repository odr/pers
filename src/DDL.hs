{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DDL where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol(..), KnownSymbol, symbolVal')
import Data.Typeable(Typeable(..))
import Data.Default(Default(..))

import NamedValue((:>))
import NamedRecord(RecStack(), Init, Last)

newtype Table (ent :: Symbol) rec pk = Table rec
    deriving (Typeable, Show, Eq, Ord, Functor
            , Traversable, Foldable, Monoid, Default)

class DDL backend a where
    createTable :: Proxy# backend -> Proxy a -> String

-- | DDL-type-information and conversion from/to type to/from database type.
--   Database type is a type specified in db-library which
--   present different db-types as a sum-type
class FieldDDL backend fdb a | backend -> fdb where
    typeName    :: Proxy# backend -> Proxy a -> String -- ^ name of type in database
    nullStr     :: Proxy# backend -> Proxy a -> String -- ^ NULL or NOT NULL
    nullStr _ _ = "NOT NULL"
    toDb        :: Proxy# backend -> a -> fdb           -- ^ value to database type
    fromDb      :: Proxy# backend -> fdb -> Maybe a     -- ^ database type to value

class RowDDL backend a where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a -> String

instance (FieldDDL b fdb v, KnownSymbol n) => RowDDL b (n :> v) where
    rowCreate pb (_ :: Proxy (n:>v))
        = symbolVal' (proxy# :: Proxy# n)
            ++ " " ++ typeName pb pv
            ++ (if null ns then "" else " " ++ ns)
      where
        pv = Proxy :: Proxy v
        ns = nullStr pb pv

instance (RecStack (x,y), RowDDL b (Last (x,y)), RowDDL b (Init (x,y))) => RowDDL b (x,y) where
    rowCreate pb _
        =  rowCreate pb (Proxy :: Proxy (Init (x,y)))
        ++ ","
        ++ rowCreate pb (Proxy :: Proxy (Last (x,y)))
{-
instance (RowDDL b x, RowDDL b y) => RowDDL b (x,y) where
    rowCreate pb _
        =  rowCreate pb (Proxy :: Proxy x)
        ++ ","
        ++ rowCreate pb (Proxy :: Proxy y)
-}
