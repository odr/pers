{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Lens.Micro(Lens', (&), (.~), (^.))
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol)
import Data.Int(Int64)
import GHC.TypeLits(SomeSymbol, someSymbolVal)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.IO.Class(MonadIO(..))
import qualified Data.Text as T
import Control.Monad.Catch(catch, SomeException)
import Control.Monad.Trans.Either(EitherT)
-- import Control.Monad.Except(ExceptT)
import Network.Wai.Handler.Warp(run)
import Network.Wai
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Types
import Servant -- (serve, (:~>)(..), type (:~>), ServantErr, enter)
import Servant.Docs

import Pers.Types -- ((:::),Rep(Plain),VRec,recLens,pNRec,recLens')
import Pers.Database.DDL -- (TableDef, runSession, DDL(..))
import Pers.Database.DML -- (DML(..),Cond(..),InsAutoPK(..),sel)
import Pers.Database.Sqlite.Sqlite(Sqlite, sqlite)
import Pers.Servant.Servant
import Pers.Servant.Simple

import Tab1
import Tab2
import Tab3

sql :: IO ()
sql = do
    runSession sqlite "test.db" (do
            createTab1
            createTab2
            createTab3
        )
  where


-- TODO обработка ошибок
-- TODO внешние ключи
-- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом
-- TODO проверить добавление через def -- произвольный порядок полей
-- TODO транзакции
-- TODO: to make conduit (or pipe) for Select

main :: IO ()
main = do
    sql
    run 8081 app

-- type Tabs = ServDatas Plain '[Tab1, Tab2, Tab3]
-- type Tabs = ServDatas Plain '[Tab3]

type MyAPI = Tab1API :<|> Tab2API :<|> Tab3API
-- PersAPI' Plain SimpleHtml Sqlite Tabs
myAPI = Proxy :: Proxy MyAPI

type DocsAPI = MyAPI :<|> "doc.md" :> Raw
api :: Proxy DocsAPI
api = Proxy

app = serve api serverD
-- app = serve myAPI server

-- runTestDB :: PersMonad Sqlite :~> ExceptT ServantErr IO
runTestDB :: PersMonad Sqlite :~> EitherT ServantErr IO
runTestDB = Nat $ runSession sqlite "test.db"

serverD :: Server DocsAPI
serverD = server :<|> serveDocs
  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS
        plain = ("Content-Type", "text/plain")

server  =       enter runTestDB serverTab1
        :<|>    enter runTestDB serverTab2
        :<|>    enter runTestDB serverTab3
-- server  = enter runTestDB
--         $ persServerSimple  (proxy# :: Proxy# Plain)
--                             (proxy# :: Proxy# Sqlite)
--                             (Proxy  :: Proxy Tabs)

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] myAPI
  where intro = DocIntro "Welcome"
                    ["This is our super webservice's API.", "Enjoy!"]
