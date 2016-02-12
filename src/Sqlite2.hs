{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Sqlite2 where

import Prelude as P
import GHC.Prim(Proxy#, proxy#)
import Control.Monad.Catch
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol(..), KnownSymbol, SomeSymbol(..), symbolVal', symbolVal)
import Data.Text(Text)
import Data.ByteString(ByteString)
import Database.SQLite3 -- (SQLData(..), Database, exec, prepare, step, bind, finalize)
import Data.Int(Int64)
import qualified Data.Text.Lazy as TL
import Data.Text.Format -- (format)
import Control.Monad.Trans.Reader(ReaderT(..), ask)
import Control.Monad.Trans.RWS(RWS(..))
import Control.Monad.IO.Class(MonadIO(..))
import Data.List(intercalate)

import NRTH
import NamedRecord2
import DDL2
import DML2

data Sqlite
sqlite = Proxy :: Proxy Sqlite

type instance Conn Sqlite = Database
type instance SessionParams Sqlite = Text
type instance FieldDB Sqlite = SQLData

instance DBOption Sqlite where
    paramName _ = format "?{}" . Only

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
        finally (runReaderT sm (sqlite, conn))
                (liftIO $ close conn)

instance (RowDDL Sqlite a, KnownSymbol n, Names pk)
    => DDL Sqlite (TableDef n a pk) -- ("table":::("name":::n,"rec":::a,"pk":::pk))
  where
    createTable (Proxy :: Proxy (TableDef n a pk))
        = runSqliteDDL
            $ format "CREATE TABLE IF NOT EXISTS {} ({}, PRIMARY KEY ({}))"
                ( symbolVal' (proxy# :: Proxy# n)
                , TL.intercalate ","
                    $ rowCreate (proxy# :: Proxy# Sqlite) (Proxy :: Proxy a) []
                , intercalate ","
                    $ names (proxy# :: Proxy# pk)
                )
    dropTable (Proxy :: Proxy (TableDef n a pk))
        = runSqliteDDL
            $ format "DROP TABLE {}" $ Only $ symbolVal' (proxy# :: Proxy# n)

runSqliteDDL :: (MonadIO m) => TL.Text -> SessionMonad Sqlite m ()
runSqliteDDL cmd = ask >>= \(_,conn) -> liftIO (exec conn $ TL.toStrict cmd)

instance AutoGenPK Sqlite Int64 where
    getPK = ask >>= \(_,conn) -> liftIO (lastInsertRowId conn)

instance (Names (NRec a), KnownSymbol n, RowDDL Sqlite a)
    => DML Sqlite (TableDef n a pk)
  where
    ins (_::Proxy (TableDef n a pk)) rs
        = do
            (_,conn) <- ask
            liftIO $ P.print (cmd, pss)
            stat <- liftIO $ prepare conn $ TL.toStrict cmd
            -- liftIO $ mapM_ (\ps -> insRow stat ps) pss
            liftIO $ finally (mapM_ (\ps -> insRow stat ps) pss)
                             (finalize stat)
      where
        (cmd, pss)
            = insRecCmdPars (proxy# :: Proxy# Sqlite) (proxy# :: Proxy# n)
                    (Proxy :: Proxy a) rs

    del (_ :: Proxy (TableDef n a pk)) c = do
        (_,conn) <- ask
        liftIO $ P.print (cmd, ps)
        liftIO $ prepare conn (TL.toStrict cmd)
                >>= \stat -> finally
                    (bind stat ps >> step stat >> changes conn)
                    (finalize stat)
      where
        (cmd,ps) = delRecCmdPars (proxy# :: Proxy# n) c

    sel (_::Proxy (TableDef t a pk)) c = do
        (_,conn) <- ask
        liftIO $ do
            P.print cmd
            P.print ps
            p <- prepare conn $ TL.toStrict cmd
            bind p ps
            finally
                (loop p id) -- TODO: to make conduit (or pipe)
                (finalize p)
      where
        (cmd,ps)
            = selRecCmdPars (proxy# :: Proxy# t) (proxy# :: Proxy# (NRec a)) c
        loop p frs = do
            res <- step p
            if res == Done
                then return (frs [])
                else fmap (\r -> frs
                        . (checkErr (fromRowDb (proxy# :: Proxy# Sqlite)
                                                (Proxy :: Proxy a)
                                                r) :))
                        (columns p >>= return <* P.print)
                    >>= loop p
          where
            checkErr er = case er of
                Left ss -> error
                    $ "Invalid Select. Error in column conversion with fields "
                    ++ intercalate ", " (map (\(SomeSymbol s) -> symbolVal s) ss)
                Right a -> a

    selProj (_::Proxy (TableDef t a pk)) (pb::Proxy b) c = do
        (_,conn) <- ask
        liftIO $ do
            P.print cmd
            P.print ps
            p <- prepare conn $ TL.toStrict cmd
            bind p ps
            finally
                (loop p id) -- TODO: to make conduit (or pipe)
                (finalize p)
      where
        (cmd,ps) = selRecCmdPars (proxy# :: Proxy# t)
                (proxy# :: Proxy# (Minus (NRec a) b)) c
        loop p frs = do
            res <- step p
            if res == Done
                then return (frs [])
                else fmap (\r -> frs
                        . (checkErr (fromRowDb  (proxy# :: Proxy# Sqlite)
                                                (Proxy :: Proxy (ProjNames a b))
                                                r
                                    ) :))
                        (columns p >>= return <* P.print)
                    >>= loop p
          where
            checkErr er = case er of
                Left ss -> error
                    $ "Invalid Select. Error in column conversion with fields "
                    ++ intercalate ", " (map (\(SomeSymbol s) -> symbolVal s) ss)
                Right a -> a

instance (KnownSymbol t, Names (NRec (MinusNames a pk))
        , AutoGenPK Sqlite (VRecRep (ProjNames a pk)), RowDDL Sqlite (MinusNames a pk))
    => InsAutoPK Sqlite (TableDef t a pk)
  where
    insAuto (_::Proxy (TableDef t a pk)) rs = do
        (_,conn) <- ask
        stat <- liftIO $ prepare conn $ TL.toStrict cmd
        finally (mapM (\ps -> liftIO (insRow stat ps) >> getPK) pss)
                (liftIO $ finalize stat)
      where
        (cmd, pss)
            = insRecCmdPars (proxy# :: Proxy# Sqlite)
                        (proxy# :: Proxy# t)
                        (Proxy :: Proxy (MinusNames a pk))
                        rs

insRow :: Statement -> [SQLData] -> IO ()
insRow stat ps = do
    reset stat
    bind stat ps
    step stat
    return ()

