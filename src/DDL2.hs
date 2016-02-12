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
module DDL2 where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol(..), KnownSymbol, symbolVal', SomeSymbol(..))
import Data.Typeable(Typeable(..))
import Data.Default(Default(..))
import Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Format(format)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Catch
import Data.Type.Equality((:~:)(..), castWith)

import Data.Promotion.Prelude.List

import NamedRecord2 -- (RecStack(..), Init, Last, (:>)(..), Diff)

{-
class Named a where
    getName :: Proxy a -> String
-}
{-
newtype Table (n :: Symbol) rec pk = Table { tableRec :: rec }
    deriving (Typeable, Show, Eq, Ord, Functor
            , Traversable, Foldable, Monoid, Default)
-}
type TableDef (n :: Symbol) (rec :: [ (Symbol, *) ]) (pk :: [Symbol])
    =  '( "table" ::: n
        , "rec" ::: rec
        , "pk" ::: pk
        )
type family Conn backend
type family FieldDB backend
type family SessionParams backend

type family PKDef (a :: k) :: [Symbol]
type family DataRecordDef (a :: k) :: [(Symbol,*)]
type family RecordDef (a :: k) :: [(Symbol,*)]

type instance DataRecordDef (TableDef n rec pk) = MinusNames rec pk

type SessionMonad b m = ReaderT (Proxy b, Conn b) m

-- | Options for backend
class DBOption back where
    paramName :: Proxy# back -> Int -> Text -- | How to create param name (like "?1") from order

--class Db backend param conn | backend -> param, backend -> conn where
--    dbInit :: Proxy# backend -> ReaderT conn IO ()
class DBSession back where
    runSession :: (MonadIO m, MonadMask m)
            => Proxy back -> SessionParams back -> SessionMonad back m a -> m a

class DDL backend a where
    createTable :: (MonadIO m) => Proxy a -> SessionMonad backend m ()
    dropTable   :: (MonadIO m) => Proxy a -> SessionMonad backend m ()

-- | DDL-type-information and conversion from/to type to/from database type.
--   Database type is a type specified in db-library which
--   present different db-types as a sum-type
class FieldDDL backend (a :: *) where
    typeName    :: Proxy# backend -> Proxy# a -> Text -- ^ name of type in database
    nullStr     :: Proxy# backend -> Proxy# a -> Text -- ^ NULL or NOT NULL
    nullStr _ _ = "NOT NULL"
    toDb        :: Proxy# backend -> a -> FieldDB backend -- ^ value to database type
    fromDb      :: Proxy# backend -> FieldDB backend -> Maybe a -- ^ database type to value

class RowDDL backend (a :: [(Symbol,*)]) where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a
                -> [Text] -> [Text]
    toRowDb     :: Proxy# backend -> Proxy a -> VRecRep a
                -> [FieldDB backend] -> [FieldDB backend]
    fromRowDb   :: Proxy# backend -> Proxy a -> [FieldDB backend]
                -> Either [SomeSymbol] (VRecRep a)

magic :: (Proxy a -> b) -> Proxy# a -> b
magic f _ = f (Proxy :: Proxy a)

instance RowDDL b ('[]) where
    rowCreate _ _   = id
    toRowDb   _ _ _ = id
    fromRowDb _ _ _ = Right ()

instance (FieldDDL b v, KnownSymbol n, RowDDL b nvs, Names (NRec nvs))
    => RowDDL b ((n ::: v) ': nvs)
  where
    rowCreate pb (_ :: Proxy ((n:::v) ': nvs))
        = (format "{} {}{}" ( symbolVal' (proxy# :: Proxy# n)
                            , typeName pb (proxy# :: Proxy# v)
                            , if TL.null ns then "" else " " `mappend` ns
                            )
        :) . rowCreate pb (Proxy :: Proxy nvs)
      where
        ns = nullStr pb (proxy# :: Proxy# v)

    toRowDb pb _ (v,_) = (toDb pb v :)
    fromRowDb pb (_ :: Proxy ((n ::: v) ': nvs)) fs
        = case fs of
            []      -> Left $ symbols (proxy# :: Proxy# (n ': NRec nvs))
            (f:fs)  -> case fromRowDb pb (Proxy :: Proxy nvs) fs of
                Left ss
                    -> either (Left . (:ss)) (\_ -> Left ss) $ rh f
                Right r
                    -> either (Left . (:[])) (\x -> Right (x,r)) $ rh f
      where
        rh f = maybe (Left $ SomeSymbol (Proxy :: Proxy n)) Right
                (fromDb pb f :: Maybe v)

rowDb :: RowDDL backend a => Proxy# backend -> Proxy a -> VRecRep a -> [FieldDB backend]
rowDb pdb pa v = toRowDb pdb pa v []

