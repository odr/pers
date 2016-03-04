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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Pers.Database.Sqlite.Sqlite where

import Prelude as P
import GHC.Prim(Proxy#, proxy#)
import Control.Monad.Catch
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol(..), KnownSymbol, SomeSymbol(..), symbolVal', symbolVal)
import Data.Text(Text)
import Data.Int(Int64)
import Data.ByteString(ByteString)
import Database.SQLite3 -- (SQLData(..), Database, exec, prepare, step, bind, finalize)
import qualified Data.Text.Lazy as TL
import Data.Text.Format -- (format)
import Control.Monad(foldM)
import Control.Monad.Trans.Reader(ReaderT(..), ask)
import Control.Monad.Trans.RWS(RWS(..))
import Control.Monad.IO.Class(MonadIO(..))
import Data.List(intercalate)
import Lens.Micro((^.))
import Control.Arrow(first)
import GHC.Exts(Constraint)

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML

data Sqlite

sqlite = Proxy :: Proxy Sqlite

instance DBOption Sqlite where
    type Conn Sqlite            = Database
    type SessionParams Sqlite   = Text
    type FieldDB Sqlite         = SQLData
    paramName _                 = format "?{}" . Only
    runSession _ par sm         = do
        liftIO $ P.print "Make Sqlite Connection!"
        conn <- liftIO $ open par
        -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
        --             (\(_::SomeException) -> return ()) -- for sqlite3
        catch (runReaderT sm (sqlite, conn) <* liftIO (close conn >> P.print "closed!!!"))
                (\(e::SomeException) -> liftIO (close conn >> P.print "closed!!!") >> throwM e)

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

instance (RowDDL Sqlite a, KnownSymbol n, Names pk, Namess uk, FromFKDef fk
        ,ContainNames a pk, CheckFK a fk, ContainNamess a uk)
    => DDL Sqlite (TableDef n a pk uk fk)
  where
    createTable (Proxy :: Proxy (TableDef n a pk uk fk))
        = runSqliteDDL
            $ format "CREATE TABLE IF NOT EXISTS {} ({}, PRIMARY KEY ({}) {} {})"
                ( symbolVal' (proxy# :: Proxy# n)
                , TL.intercalate ","
                    $ rowCreate (proxy# :: Proxy# Sqlite) (Proxy :: Proxy a) []
                , intercalate ","
                    $ names (proxy# :: Proxy# pk)
                , foldMap (format ",UNIQUE ({})" . Only . intercalate ",")
                    $ namess (proxy# :: Proxy# uk)
                , foldMap ( format ",FOREIGN KEY ({}) REFERENCES {} ({})"
                          . ((,,) <$> intercalate "," . fst . fst
                                  <*> fst . snd
                                  <*> intercalate "," . snd . fst
                            )
                          . first unzip
                          )
                    $ fromFKDef (proxy# :: Proxy# fk)
                )
    dropTable (Proxy :: Proxy (TableDef n a pk uk fk))
        = runSqliteDDL
            $ format "DROP TABLE {}" $ Only $ symbolVal' (proxy# :: Proxy# n)

runSqliteDDL :: (MonadIO m) => TL.Text -> SessionMonad Sqlite m ()
runSqliteDDL cmd = do
    liftIO $ P.print cmd
    ask >>= \(_,conn) -> liftIO (exec conn $ TL.toStrict cmd)

type family IsInt64 (pk :: [(k,*)]) :: Constraint where
    IsInt64 '[ '(x,Int64)] = ()

type instance IsAutoPK rep Sqlite kr = kr ~ Singl rep Int64

instance    ( Names (NRec a)
            , KnownSymbol n
            , Single rep
            , ContainNames a pk
            , RecLens rep a (ProjNames a pk) ar kr
            , RecLens rep a (MinusNames a pk) ar dr
            , Names (NRec (MinusNames a pk))
            , Names pk
            , RowRepDDL rep Sqlite a ar
            , RowRepDDL rep Sqlite (ProjNames a pk) kr
            , RowRepDDL rep Sqlite (MinusNames a pk) dr
            )
    => DML rep Sqlite (TableDef n a pk uk fk) ar kr dr
  where
    ins _ rs
        = do
            liftIO $ putStrLn "ins!"
            (_,conn) <- ask
            liftIO $ P.print (cmd, pss)
            stat <- liftIO $ prepare conn $ TL.toStrict cmd
            liftIO $ finally (mapM_ (\ps -> stepRow stat ps) pss)
                             (finalize stat)
      where
        (cmd, pss) = insRecCmdPars (Proxy :: Proxy '(rep,Sqlite,n,a)) rs

    insAuto (_::Proxy '(rep, TableDef t a pk uk fk)) rs = do
        (_,conn) <- ask
        stat <- liftIO $ prepare conn $ TL.toStrict cmd
        catch   ( mapM  (\ps -> liftIO $ stepRow stat ps
                        >> fmap (single (proxy# :: Proxy# rep))
                                (lastInsertRowId conn)
                        ) pss
                    <* liftIO (finalize stat)
                )
                (\(e::SomeException) -> liftIO (finalize stat) >> throwM e)
      where
        (cmd, pss)= insRecCmdPars
                        (Proxy :: Proxy '(rep,Sqlite,t,MinusNames a pk))
                        rs

    upd p1 rs
        = do
            liftIO $ putStrLn "upd!"
            (_,conn) <- ask
            liftIO $ P.print (cmd, pss)
            stat <- liftIO $ prepare conn $ TL.toStrict cmd
            liftIO $ finally
                (fmap ($ [])
                    $ foldM (\f (r,ps) -> do
                            stepRow stat ps
                            cnt <- changes conn
                            return $
                                if cnt == 1 then f.(r ^. (lensPk p1) :) else f
                        ) id $ zip rs pss
                )
                (finalize stat)
      where
        p2 = Proxy :: Proxy '(rep,Sqlite,TableDef n a pk uk fk)
        (cmd,pss) = updRecCmdPars p2 rs

    del _ c = do
        liftIO $ putStrLn "del!"
        (_,conn) <- ask
        liftIO $ P.print (cmd, ps)
        liftIO $ prepare conn (TL.toStrict cmd)
                >>= \stat -> finally
                    (bind stat ps >> step stat >> changes conn)
                    (finalize stat)
      where
        (cmd,ps) = delRecCmdPars (proxy# :: Proxy# n) c

    selProj (_::Proxy '(rep,TableDef t a pk uk fk,b)) c = do
        liftIO $ putStrLn "sel!"
        (_,conn) <- ask
        liftIO $ do
            P.print (cmd, ps)
            p <- prepare conn $ TL.toStrict cmd
            bind p ps
            finally
                (loop p id) -- TODO: to make conduit (or pipe)
                (finalize p)
      where
        (cmd,ps) = selRecCmdPars (Proxy :: Proxy '(rep,t,b)) c
        loop p frs = do
            res <- step p
            if res == Done
                then return (frs [])
                else fmap (\r -> frs
                        . (checkErr (fromRowDb  (proxy# :: Proxy# '(rep,Sqlite))
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

stepRow :: Statement -> [SQLData] -> IO ()
stepRow stat ps = do
    reset stat
    bind stat ps
    step stat
    return ()


