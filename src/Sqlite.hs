{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Sqlite where

import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol(..), KnownSymbol, SomeSymbol(..), symbolVal', symbolVal)
import Data.Text(Text)
import Data.ByteString(ByteString)
import Database.SQLite3 -- (SQLData(..), Database, exec, prepare, step, bind, finalize)
import Data.Int(Int64)
import qualified Data.Text.Lazy as TL
import Data.Text.Format -- (format)
import Control.Monad.Trans.Reader(ReaderT(..), ask)
import Control.Monad.IO.Class(MonadIO(..))

import NamedRecord
import DDL
import DML

data Sqlite
sqlite = Proxy :: Proxy Sqlite

type instance Conn Sqlite = Database
type instance SessionParams Sqlite = Text
type instance FieldDB Sqlite = SQLData

instance (FieldDDL Sqlite a) => FieldDDL Sqlite (Maybe a) where
    typeName pb (_::Proxy (Maybe a))
                        = typeName pb (Proxy :: Proxy a)
    toDb pb (Just a)    = toDb pb a
    toDb _ Nothing      = SQLNull
    fromDb _ SQLNull    = Just Nothing
    fromDb pb a         = fmap Just $ fromDb pb a
    nullStr _ _         = ""

instance FieldDDL Sqlite Int64 where
    typeName _ _            = "INTEGER"
    toDb _ a                = SQLInteger a
    fromDb _ (SQLInteger a) = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite Text where
    typeName _ _            = "TEXT"
    toDb _ a                = SQLText a
    fromDb _ (SQLText a)    = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite Double where
    typeName _ _            = "FLOAT"
    toDb _ a                = SQLFloat a
    fromDb _ (SQLFloat a)    = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite ByteString where
    typeName _ _            = "BLOB"
    toDb _ a                = SQLBlob a
    fromDb _ (SQLBlob a)    = Just a
    fromDb _ _              = Nothing

instance DBSession Sqlite where
    runSession _ par sm = do
        conn <- liftIO $ open par
        r <- runReaderT sm (sqlite, conn)
        liftIO $ close conn
        return r

instance (RowDDL Sqlite a, KnownSymbol n, NamesList pk)
    => DDL Sqlite (Table n a pk)
  where
    createTable (Proxy :: Proxy (Table n a pk)) = runSqliteDDL
        $ format "CREATE TABLE IF NOT EXISTS {} ({}, PRIMARY KEY ({}))"
            ( symbolVal' (proxy# :: Proxy# n)
            , rowCreate sqlite (Proxy :: Proxy a)
            , namesStr (proxy# :: Proxy# pk)
            )
    dropTable (Proxy :: Proxy (Table n a pk)) = runSqliteDDL
        $ format "DROP TABLE {}" $ Only $ symbolVal' (proxy# :: Proxy# n)

runSqliteDDL :: (MonadIO m) => TL.Text -> SessionMonad Sqlite m ()
runSqliteDDL cmd = ask >>= \(_,conn) -> liftIO (exec conn $ TL.toStrict cmd)

instance (NamesList a, KnownSymbol t, RowDDL Sqlite a)
    => Ins Sqlite (Table t a pk)
  where
    ins (Table rec :: Table t a pk) = insSqlite (proxy# :: Proxy# t) rec

instance AutoGenPK Sqlite Int64 where
    getPK = ask >>= \(_,conn) -> liftIO (lastInsertRowId conn)

instance (KnownSymbol t, NamesList (Diff a pk)
        , AutoGenPK Sqlite pk, RowDDL Sqlite (Diff a pk))
    => InsAutoPK Sqlite (Table t a pk)
  where
    insAuto _ (rec :: DataRow (Table t a pk))
        = insSqlite (proxy# :: Proxy# t) rec >> getPK

insSqlite :: (KnownSymbol t, RowDDL Sqlite r, NamesList r, MonadIO m)
    => Proxy# (t::Symbol) -> r -> SessionMonad Sqlite m ()
insSqlite pt (rec :: r) = do
    (_,conn) <- ask
    liftIO $ do
        p <- prepare conn $ TL.toStrict cmd
        bind p $ rowDb sqlite rec
        step p
        finalize p
  where
    ns = namesStrL (proxy# :: Proxy# r)
    cmd = format "INSERT INTO {} ({}) VALUES({})"
        ( symbolVal' pt
        , TL.intercalate "," $ map TL.pack ns
        , TL.intercalate ", "
            $ zipWith (\n -> const $ "?" `mappend` TL.pack (show n)) [1..] ns
        )
