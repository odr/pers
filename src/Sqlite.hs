{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
module Sqlite where

import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol(..), KnownSymbol, SomeSymbol(..), symbolVal', symbolVal)
import Data.Text(Text)
import Data.ByteString(ByteString)
import Database.SQLite3(SQLData(..))
import Data.Int(Int64)

import NamedRecord(NamesList(..), namesStr, Has())
import DDL(Table(..), DDL(..), FieldDDL(..), RowDDL(..))

data Sqlite
-- sqlite = Proxy :: Proxy Sqlite

instance (FieldDDL Sqlite SQLData a) => FieldDDL Sqlite SQLData (Maybe a) where
    typeName pb (_::Proxy (Maybe a))
                        = typeName pb (Proxy :: Proxy a)
    toDb pb (Just a)    = toDb pb a
    toDb _ Nothing      = SQLNull
    fromDb _ SQLNull    = Just Nothing
    fromDb pb a         = fmap Just $ fromDb pb a
    nullStr _ _         = ""

instance FieldDDL Sqlite SQLData Int64 where
    typeName _ _            = "INTEGER"
    toDb _ a                = SQLInteger a
    fromDb _ (SQLInteger a) = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite SQLData Text where
    typeName _ _            = "TEXT"
    toDb _ a                = SQLText a
    fromDb _ (SQLText a)    = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite SQLData Double where
    typeName _ _            = "FLOAT"
    toDb _ a                = SQLFloat a
    fromDb _ (SQLFloat a)    = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite SQLData ByteString where
    typeName _ _            = "BLOB"
    toDb _ a                = SQLBlob a
    fromDb _ (SQLBlob a)    = Just a
    fromDb _ _              = Nothing

instance (RowDDL Sqlite a, KnownSymbol ent, NamesList pk, Has a pk ~ True)
    => DDL Sqlite (Table ent a pk) where
-- CREATE TABLE IF NOT EXISTS test (id INTEGER, str TEXT, PRIMARY KEY (id))
    createTable _ (Proxy :: Proxy (Table ent a pk))
        = "CREATE TABLE IF NOT EXISTS "
        ++ symbolVal' (proxy# :: Proxy# ent)
        ++ " ("
        ++ rowCreate (proxy# :: Proxy# Sqlite) (Proxy :: Proxy a)
        ++ ", PRIMARY KEY ("
        ++ namesStr (Proxy :: Proxy pk)
        ++ ")"
        ++ ")"
