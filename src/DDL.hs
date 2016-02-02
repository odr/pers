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

import NamedRecord -- (RecStack(..), Init, Last, (:>)(..), Diff)

{-
class Named a where
    getName :: Proxy a -> String
-}

newtype Table (n :: Symbol) rec pk = Table { tableRec :: rec }
    deriving (Typeable, Show, Eq, Ord, Functor
            , Traversable, Foldable, Monoid, Default)

{-
instance (KnownSymbol n) => Named (Table n rec pk) where
    getName (_ :: Proxy (Table n rec pk)) = symbolVal' (proxy# :: Proxy# n)
-}

type family Conn backend
type family FieldDB backend
type family SessionParams backend

type family PK a
type family DataRecord a
type family Record a

type instance PK (Table n rec pk) = pk
type instance DataRecord (Table n rec pk) = Diff rec pk
type instance Record (Table n rec pk) = rec

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
class FieldDDL backend a where
    typeName    :: Proxy# backend -> Proxy a -> Text  -- ^ name of type in database
    nullStr     :: Proxy# backend -> Proxy# a -> Text -- ^ NULL or NOT NULL
    nullStr _ _ = "NOT NULL"
    toDb        :: Proxy# backend -> a -> FieldDB backend -- ^ value to database type
    fromDb      :: Proxy# backend -> FieldDB backend -> Maybe a -- ^ database type to value

class RowDDL backend a where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a -> Text
    toRowDb     :: Proxy# backend -> a -> [FieldDB backend] -> [FieldDB backend]
    fromRowDb   :: Proxy# backend -> [FieldDB backend] -> Either [SomeSymbol] a

instance (FieldDDL b v, KnownSymbol n) => RowDDL b (n :> v) where
    rowCreate pb (_ :: Proxy (n:>v))
        = format "{} {}{}"  ( symbolVal' (proxy# :: Proxy# n)
                            , typeName pb (Proxy :: Proxy v)
                            , if TL.null ns then "" else " " `mappend` ns
                            )
      where
        ns = nullStr pb (proxy# :: Proxy# v)
    toRowDb pb (V v) = (toDb pb v :)
    fromRowDb pb fs = case fs of
        []      -> err
        (f:_)   -> maybe err (Right . V) (fromDb pb f :: Maybe v)
      where
        err = Left [SomeSymbol (Proxy :: Proxy n)]

instance (RecStack (x,y), RowDDL b (Last (x,y)), RowDDL b (Init (x,y))
            , (Init (x,y) +> Last (x,y)) ~ (x,y)
            , NamesList (Init (x,y))
            , NamesList (Last (x,y))
            , AddRec (Init (x,y)) (Last (x,y))
        )
        => RowDDL b (x,y) where
    rowCreate pb _
        =  format "{},{}"   ( rowCreate pb (Proxy :: Proxy (Init (x,y)))
                            , rowCreate pb (Proxy :: Proxy (Last (x,y)))
                            )
    toRowDb pb v = let (i,l) = recInitLast v in toRowDb pb i . toRowDb pb l
    fromRowDb pb ffs = case ffs of
        []      -> Left $ names (proxy# :: Proxy# (x,y))
        (ffs)   -> case ( fromRowDb pb (init ffs) :: Either [SomeSymbol] (Init (x,y))
                        , fromRowDb pb [last ffs]  :: Either [SomeSymbol] (Last (x,y))
                        ) of
            (Left r1, Left r2) -> Left $ r1 ++ r2
            (Left r1, Right _) -> Left r1
            (Right _, Left r2) -> Left r2
            (Right r1, Right r2) -> Right $ r1 +> r2

rowDb :: RowDDL backend a => Proxy# backend -> a -> [FieldDB backend]
rowDb pdb v = toRowDb pdb v []

