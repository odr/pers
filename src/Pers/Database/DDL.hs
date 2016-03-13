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
{-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE UndecidableSuperClasses #-}
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
import Lens.Micro(Lens', (^.))
import Lens.Micro.Type(Getting)
import GHC.Exts(Constraint)
import Data.Tagged

import Pers.Types

data RefType = Parent   -- ^ reference to master data. Delete "Cascade"
             | Ref      -- ^ reference to dictionary.
                        -- Delete "Restricted" or "Set Null" if possible
-- data SRefType b where
--   SParent   :: SRefType Parent
--   SRef      :: SRefType Ref
class GetRefType (r::RefType) where
    getRefType :: Proxy# (r :: RefType) -> RefType
instance GetRefType Parent  where getRefType _ = Parent
instance GetRefType Ref     where getRefType _ = Ref

data DataDef k
    = TableDef  { name  :: Symbol
                , rec   :: [(Symbol,k)]
                , pk    :: [Symbol]
                , uk    :: [[Symbol]]
                , fk    :: [([(Symbol,Symbol)],(Symbol,RefType))]
                }

instance (ToPairs '(rep, r) v) => ToPairs '(rep, TableDef n r p u f) v
  where
    toPairs = toPairs
            . (retag :: Tagged '(rep, TableDef n r p u f) v
                     -> Tagged '(rep, r) v
              )



class TableLike (a::k) where
    type TabName    (a :: k) :: Symbol
    type KeyDef     (a :: k) :: [Symbol]
    type RecordDef  (a :: k) :: [(Symbol,*)]
    type UniqDef    (a :: k) :: [[Symbol]]
    type FKDef      (a :: k) :: [([(Symbol,Symbol)],(Symbol,RefType))]

type Key a              = ProjNames  (RecordDef a) (KeyDef a)
type DataRecord a       = MinusNames (RecordDef a) (KeyDef a)

type family Keys (as::[k]) :: [[(Symbol,*)]] where
    Keys '[] = '[]
    Keys (x ': xs) = Key x ': Keys xs

type family DataRecords (as::[k]) :: [[(Symbol,*)]] where
    DataRecords '[] = '[]
    DataRecords (x ': xs) = DataRecord x ': DataRecords xs

type family RecordDefs (as::[k]) :: [[(Symbol,*)]] where
    RecordDefs '[] = '[]
    RecordDefs (x ': xs) = RecordDef x ': RecordDefs xs


instance TableLike  (TableDef n rec pk uk fk :: DataDef *) where
    type TabName    (TableDef n rec pk uk fk) = n
    type RecordDef  (TableDef n rec pk uk fk) = rec
    type KeyDef     (TableDef n rec pk uk fk) = pk
    type UniqDef    (TableDef n rec pk uk fk) = uk
    type FKDef      (TableDef n rec pk uk fk) = fk

type SessionMonad b m = ReaderT (Proxy b, Conn b) m

-- | Options for backend
class DBOption back where
    type Conn back
    type FieldDB back
    type SessionParams back
    paramName :: Proxy# back -> Int -> Text -- ^ How to create param name (like "?1") from param num
    runSession :: (MonadIO m, MonadCatch m)
            => Proxy back -> SessionParams back
            -> SessionMonad back m a -> m a

class   ( TableLike a
        , ContainNames (RecordDef a) (KeyDef a)
        , CheckFK (RecordDef a) (FKDef a)
        , ContainNamess (RecordDef a) (UniqDef a)
        )
        => DDL backend a where
    createTable :: (MonadIO m) => Proxy a -> SessionMonad backend m ()
    dropTable   :: (MonadIO m) => Proxy a -> SessionMonad backend m ()

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

instance (FieldDDL b v, KnownSymbol n, RowDDL b nvs)
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

-- lensPk ::(Functor f, RecLens rep (RecordDef a) (Key a) br ar)
--         => Proxy ('(rep,a)) -> (ar -> f ar) -> br -> f br
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

class Names2 (x :: [(Symbol,Symbol)]) where
    names2 :: Proxy# x -> [(String,String)]
instance Names2 '[] where names2 _ = []
instance (Names2 xs, KnownSymbol a, KnownSymbol b) => Names2 ( '(a,b) ': xs)
  where
    names2 _ = (symbolVal' (proxy# :: Proxy# a), symbolVal' (proxy# :: Proxy# b))
                : names2 (proxy# :: Proxy# xs)
class FromFKDef (x :: [([(Symbol,Symbol)],(Symbol,RefType))]) where
    fromFKDef :: Proxy# x -> [([(String,String)], (String,RefType))]
instance FromFKDef '[] where fromFKDef _ = []
instance (Names2 as, KnownSymbol a, FromFKDef xs
        , GetRefType r) => FromFKDef ( '(as,'(a,r)) ': xs)
  where
    fromFKDef _ =   ( names2 (proxy# :: Proxy# as)
                    ,   ( symbolVal' (proxy# :: Proxy# a)
                        , getRefType (proxy# :: Proxy# r)
                        )
                    )
                : fromFKDef (proxy# :: Proxy# xs)

type family CheckFK (a :: [(k,k2)]) (b :: [([(k,k3)],k4)]) :: Constraint where
    CheckFK a '[] = ()
    CheckFK a ('(bs,b) ': cs) = (ContainsFst a bs, CheckFK a cs)

