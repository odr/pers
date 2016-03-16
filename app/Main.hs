{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Concurrent(forkIO)
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
import qualified Language.Javascript.JQuery as JQ

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

-- TODO обработка ошибок
-- TODO внешние ключи
-- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом
-- TODO проверить добавление через def -- произвольный порядок полей
-- TODO транзакции
-- TODO: to make conduit (or pipe) for Select

main :: IO ()
main = do
    forkIO sql
    site

site = do
    forkIO writeJQ
    mapM_ (forkIO)  [ mkJsAPI pTab1API'
                    , mkJsAPI pTab2API'
                    , mkJsAPI pTab3API'
                    ]
    run 8081 app

type MyAPI = Tab1API :<|> Tab2API :<|> Tab3API
myAPI = Proxy :: Proxy MyAPI

type DocsAPI = MyAPI
            :<|> "doc.md" :> Raw
            :<|> "static" :> Raw
api :: Proxy DocsAPI
api = Proxy

app = serve api serverD

runTestDB :: PersMonad Sqlite :~> EitherT ServantErr IO
runTestDB = Nat $ runSession sqlite "test.db"

serverD :: Server DocsAPI
serverD = server :<|> serveDocs :<|> serveDirectory "js"
  where serveDocs _ respond =
            respond $ responseLBS ok200 [plain] docsBS
        plain = ("Content-Type", "text/plain")

server  =       enter runTestDB serverTab1
        :<|>    enter runTestDB serverTab2
        :<|>    enter runTestDB serverTab3

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] myAPI
  where intro = DocIntro "Welcome"
                    ["This is our super webservice's API.", "Enjoy!"]

instance ToCapture (Capture "id" Int64) where
  toCapture _ = DocCapture "id" "identity of record"

instance ToCapture (Capture "t1_id" Int64) where
  toCapture _ = DocCapture "t1_id" "identity of record in table t1"

instance ToCapture (Capture "t2_id" Int64) where
  toCapture _ = DocCapture "t2_id" "identity of record in table t2"

writeJQ :: IO ()
writeJQ = do
  jq <- JQ.file >>= readFile
  writeFile "js/jq.js" jq
