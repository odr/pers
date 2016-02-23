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
import Data.Promotion.Prelude.List

import Pers.Types -- (RecStack(..), Init, Last, (:>)(..), Diff)

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
    = "table" ::: '("name" ::: n, "rec"::: rec, "pk" ::: pk)

type family Conn backend
type family FieldDB backend
type family SessionParams backend

type family TabName (a :: k)        :: Symbol
type family KeyDef (a :: k)         :: [Symbol]
type family DataRecordDef (a :: k)  :: [(Symbol,*)]
type family RecordDef (a :: k)      :: [(Symbol,*)]

type instance TabName (TableDef n rec pk)       = n
type instance KeyDef (TableDef n rec pk)        = pk
type instance DataRecordDef (TableDef n rec pk) = MinusNames rec pk
type instance RecordDef (TableDef n rec pk)     = rec

type Key a = ProjNames (RecordDef a) (KeyDef a)

{-
class ( (Key t :\\ RecordDef t) ~ '[]
        , NRec (Key t) ~ KeyDef t)
      ) => DBRule t where
-}

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
    -- HasDef (n:>v) = HasDef v
    HasDef (a,b) = HasDef a && HasDef b
    HasDef () = True
    HasDef a = False


class RowDDL backend (a :: [(Symbol,*)]) where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a
                -> [Text] -> [Text]

class RowRepDDL (rep::Rep) back (a::[(Symbol,*)]) where
    toRowDb     :: Proxy# '(rep,back) -> Proxy a -> VRec rep a
                -> [FieldDB back] -> [FieldDB back]
    fromRowDb   :: Proxy# '(rep,back) -> Proxy a -> [FieldDB back]
                -> Either [SomeSymbol] (VRec rep a)

-- magic :: (Proxy a -> b) -> Proxy# a -> b
-- magic f _ = f (Proxy :: Proxy a)

instance RowDDL b ('[]) where
    rowCreate _ _   = id

instance RowRepDDL Plain b ('[]) where
    toRowDb   _ _ _ = id
    fromRowDb _ _ _ = Right ()

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

instance (FieldDDL b v, KnownSymbol n, RowRepDDL Plain b nvs, Names (NRec nvs))
    => RowRepDDL Plain b ((n ::: v) ': nvs)
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

rowDb :: RowRepDDL rep backend a => Proxy# '(rep, backend) -> Proxy a
        -> VRec rep a -> [FieldDB backend]
rowDb prb pa v = toRowDb prb pa v []

-- lensPk :: (Functor f, RecLens rep (RecordDef a) (Key a))
--     => Proxy '(rep,a) -> Lens' (VRec rep (RecordDef a)) (VRec rep (Key a))
lensPk (_::Proxy '(rep,a))
    = recLens (proxy#::Proxy#  '( rep, RecordDef a, Key a ))

lensData (_::Proxy '(rep,a))
    = recLens (proxy#::Proxy#  '( rep, RecordDef a, DataRecordDef a ))

-- recPk :: (RecLens rep (RecordDef a) (Key a))
--     => Proxy '(rep,a) -> (VRec rep (Key a)) -> (VRec rep (RecordDef a))
-- recPk   p r = lensPk   p ^. r
-- recData p r = lensData p ^. r


{-
recDbPk
    :: (Functor f, RecLens rep (RecordDef a) (Key a), RowRepDDL rep back (Key a))
    => Proxy '(rep, back, a)
--         -> VRec rep (RecordDef a)
        -> Getting
              (VRec rep (Key a))
              ((VRec rep (Key a) -> f (VRec rep (Key a)))
                -> VRec rep (RecordDef a) -> f (VRec rep (RecordDef a)))
              (VRec rep (Key a))
        -> [FieldDB back]
-}
recDbPk :: (RecLens rep (RecordDef a) (Key a), RowRepDDL rep back (Key a))
    => Proxy '(rep, back, a) -> VRec rep (RecordDef a) -> [FieldDB back]
recDbPk (_::Proxy '(rep,back,a)) rec -- :: VRec rep (RecordDef a))
    = rowDb (proxy# :: Proxy# '(rep,back))
            (Proxy :: Proxy (Key a))
            (rec ^. lensPk (Proxy :: Proxy '(rep,a)))

recDbData :: (RecLens rep (RecordDef a) (DataRecordDef a)
            , RowRepDDL rep back (DataRecordDef a))
    => Proxy '(rep, back, a) -> VRec rep (RecordDef a) -> [FieldDB back]
recDbData (_::Proxy '(rep,back,a)) rec -- :: VRec rep (RecordDef a))
    = rowDb (proxy# :: Proxy# '(rep,back))
            (Proxy :: Proxy (DataRecordDef a))
            (rec ^. lensData (Proxy :: Proxy '(rep,a)))
