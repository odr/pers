{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE RankNTypes #-}
module Pers.Database.DDL where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol(..), KnownSymbol, symbolVal', SomeSymbol(..))
import Data.Type.Bool(type (&&), type (||), If)
import Data.Typeable(Typeable(..))
import Data.Default(Default(..))
import Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Format(format)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Catch
import Data.Type.Equality((:~:)(..), castWith)
-- import Control.Lens(Lens', (^.))
-- import Control.Lens.Getter(Getting)
import Lens.Micro(Lens', (^.))
import Lens.Micro.Type(Getting)
-- import Data.Promotion.Prelude.List

import Pers.Types

type TableDef (n :: Symbol) (rec :: [ (Symbol, *) ]) (pk :: [Symbol])
    = "table" ::: '("name" ::: n, "rec"::: rec, "pk" ::: pk)

class TableLike (a::k) where
    type TabName (a :: k)   :: Symbol
    type KeyDef (a :: k)    :: [Symbol]
    type RecordDef (a :: k) :: [(Symbol,*)]

type Key a              = ProjNames  (RecordDef a) (KeyDef a)
type DataRecord a       = MinusNames (RecordDef a) (KeyDef a)

instance TableLike  (TableDef n rec pk) where
    type TabName    (TableDef n rec pk) = n
    type KeyDef     (TableDef n rec pk) = pk
    type RecordDef  (TableDef n rec pk) = rec

type SessionMonad b m = ReaderT (Proxy b, Conn b) m

-- | Options for backend
class DBOption back where
    type Conn back
    type FieldDB back
    type SessionParams back
    paramName :: Proxy# back -> Int -> Text -- ^ How to create param name (like "?1") from order
    runSession :: (MonadIO m, MonadCatch m)
            => Proxy back -> SessionParams back -> SessionMonad back m a -> m a

class DDL backend a where
    createTable :: (MonadIO m) => Proxy a -> SessionMonad backend m ()
    dropTable   :: (MonadIO m) => Proxy a -> SessionMonad backend m ()

-- newtype Field back a = Field a
-- | DDL-type-information and conversion from/to type to/from database type.
--   Database type is a type specified in db-library which
--   present different db-types as a sum-type
class FieldDDL backend (a :: *) where
    typeName    :: Proxy# backend -> Proxy a -> Text -- ^ name of type in database
    nullStr     :: Proxy# backend -> Proxy# a -> Text -- ^ NULL or NOT NULL
    nullStr _ _ = "NOT NULL"
    toDb        :: Proxy# backend -> a -> FieldDB backend -- ^ value to database type
    fromDb      :: Proxy# backend -> FieldDB backend -> Maybe a -- ^ database type to value

-- | Does this type can have default (null) value.
--   True for Maybe and [] but not for String.
type family HasDef a :: Bool where
    HasDef (Maybe a) = True
    HasDef String = False
    HasDef [a] = True
    HasDef (a,b) = HasDef a && HasDef b
    HasDef () = True
    HasDef a = False


class RowDDL backend (a :: [(Symbol,*)]) where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a
                -> [Text] -> [Text]

instance RowDDL b ('[]) where
    rowCreate _ _   = id

instance (FieldDDL b v, KnownSymbol n, RowDDL b nvs, Names (NRec nvs))
    => RowDDL b ((n ::: v) ': nvs)
  where
    rowCreate pb (_ :: Proxy ((n:::v) ': nvs))
        = (format "{} {}{}" ( symbolVal' (proxy# :: Proxy# n)
                            , typeName pb (Proxy :: Proxy v)
                            , if TL.null ns then "" else " " `mappend` ns
                            )
        :) . rowCreate pb (Proxy :: Proxy nvs)
      where
        ns = nullStr pb (proxy# :: Proxy# v)

class (Rep rep a ar)
    => RowRepDDL (rep::R) back (a::[(Symbol,*)]) ar | rep a -> ar
  where
    toRowDb     :: Proxy# '(rep,back) -> Proxy a -> ar
                -> [FieldDB back] -> [FieldDB back]
    fromRowDb   :: Proxy# '(rep,back) -> Proxy a -> [FieldDB back]
                -> Either [SomeSymbol] ar

instance RowRepDDL Plain b ('[]) () where
    toRowDb   _ _ _ = id
    fromRowDb _ _ _ = Right ()

instance    ( FieldDDL b v
            , KnownSymbol n
            , RowRepDDL Plain b nvs vr
            , Names (NRec nvs)
            )
    => RowRepDDL Plain b ((n ::: v) ': nvs) (v,vr)
  where
    toRowDb prb _ (v,vs) = (toDb (proxy# :: Proxy# b) v :)
                         . toRowDb prb (Proxy :: Proxy nvs) vs
    fromRowDb prb (_ :: Proxy ((n ::: v) ': nvs)) fs
        = case fs of
            []      -> Left $ symbols (proxy# :: Proxy# (n ': NRec nvs))
            (f:fs)  -> case fromRowDb prb (Proxy :: Proxy nvs) fs of
                Left ss -> either (Left . (:ss)) (\_ -> Left ss) $ rh f
                Right r -> either (Left . (:[])) (\x -> Right (x,r)) $ rh f
      where
        rh f = maybe (Left $ SomeSymbol (Proxy :: Proxy n)) Right
                (fromDb (proxy# :: Proxy# b) f :: Maybe v)

rowDb :: (RowRepDDL rep backend a ar)
        => Proxy# '(rep, backend) -> Proxy a -> ar -> [FieldDB backend]
rowDb prb pa v = toRowDb prb pa v []

lensPk (_::Proxy '(rep,a))
    = recLens (proxy#::Proxy#  '( rep, RecordDef a, Key a ))
lensData (_::Proxy '(rep,a))
    = recLens (proxy#::Proxy#  '( rep, RecordDef a, DataRecord a ))

recDbPk ::  ( RecLens rep (RecordDef a) (Key a) s ar
            , RowRepDDL rep back (Key a) ar
            )
    => Proxy '(rep, back, a) -> s -> [FieldDB back]
recDbPk (_::Proxy '(rep,back,a)) rec
    = rowDb (proxy# :: Proxy# '(rep,back))
            (Proxy :: Proxy (Key a))
            (rec ^. lensPk (Proxy :: Proxy '(rep,a)))

recDbData   ::  ( RecLens rep (RecordDef a) (DataRecord a) s ar
                , RowRepDDL rep back (DataRecord a) ar
                )
    => Proxy '(rep, back, a) -> s -> [FieldDB back]
recDbData (_::Proxy '(rep,back,a)) rec
    = rowDb (proxy# :: Proxy# '(rep,back))
            (Proxy :: Proxy (DataRecord a))
            (rec ^. lensData (Proxy :: Proxy '(rep,a)))
