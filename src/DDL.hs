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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module DDL where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol(..), KnownSymbol, symbolVal')
import Data.Typeable(Typeable(..))
import Data.Default(Default(..))
import Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Format(format)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Trans.Reader(ReaderT)

import NamedRecord(RecStack(..), Init, Last, (:>)(..), Diff)

newtype Table (n :: Symbol) rec pk = Table rec
    deriving (Typeable, Show, Eq, Ord, Functor
            , Traversable, Foldable, Monoid, Default)

type family Conn backend
type family FieldDB backend
type family SessionParams backend

type family PK a where
    PK (Table n rec pk) = pk

type family DataRow a where
    DataRow (Table n rec pk) = Diff rec pk

{-
type family DataRowWithPK a where
    DataRowWithPK (Table n rec pk) = rec
-}

type SessionMonad b m = ReaderT (Proxy b, Conn b) m

--class Db backend param conn | backend -> param, backend -> conn where
--    dbInit :: Proxy# backend -> ReaderT conn IO ()

class DBSession back where
    runSession :: (MonadIO m) => Proxy back -> SessionParams back -> SessionMonad back m a -> m a

class DDL backend a where
    createTable :: (MonadIO m) => Proxy a -> SessionMonad backend m ()
    dropTable   :: (MonadIO m) => Proxy a -> SessionMonad backend m ()

-- | DDL-type-information and conversion from/to type to/from database type.
--   Database type is a type specified in db-library which
--   present different db-types as a sum-type
class FieldDDL backend a where
    typeName    :: Proxy backend -> Proxy a -> Text  -- ^ name of type in database
    nullStr     :: Proxy backend -> Proxy# a -> Text -- ^ NULL or NOT NULL
    nullStr _ _ = "NOT NULL"
    toDb        :: Proxy backend -> a -> FieldDB backend -- ^ value to database type
    fromDb      :: Proxy backend -> FieldDB backend -> Maybe a -- ^ database type to value

class RowDDL backend a where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy backend -> Proxy a -> Text
    toRowDb     :: Proxy backend -> a -> [FieldDB backend] -> [FieldDB backend]

instance (FieldDDL b v, KnownSymbol n) => RowDDL b (n :> v) where
    rowCreate pb (_ :: Proxy (n:>v))
        = format "{} {}{}"  ( symbolVal' (proxy# :: Proxy# n)
                            , typeName pb (Proxy :: Proxy v)
                            , if TL.null ns then "" else " " `mappend` ns
                            )
      where
        ns = nullStr pb (proxy# :: Proxy# v)
    toRowDb pb (V v) = (toDb pb v :)

instance (RecStack (x,y), RowDDL b (Last (x,y)), RowDDL b (Init (x,y)))
        => RowDDL b (x,y) where
    rowCreate pb _
        =  format "{},{}"   ( rowCreate pb (Proxy :: Proxy (Init (x,y)))
                            , rowCreate pb (Proxy :: Proxy (Last (x,y)))
                            )
    toRowDb pb v = let (i,l) = recInitLast v in toRowDb pb i . toRowDb pb l

rowDb :: RowDDL backend a => Proxy backend -> a -> [FieldDB backend]
rowDb pdb v = toRowDb pdb v []

