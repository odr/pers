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
import Servant -- (serve, (:~>)(..), type (:~>), ServantErr, enter)
import Servant.Docs
import Network.Wai.Handler.Warp(run)
import Network.Wai
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Types

import Pers.Types -- ((:::),Rep(Plain),VRec,recLens,pNRec,recLens')
import Pers.Database.DDL -- (TableDef, runSession, DDL(..))
import Pers.Database.DML -- (DML(..),Cond(..),InsAutoPK(..),sel)
import Pers.Database.Sqlite.Sqlite(Sqlite, sqlite)
import Pers.Servant.Servant
import Pers.Servant.Simple
-- import Pers.Servant.Lucid()

import Tab1

sql :: IO ()
sql = do
    runSession sqlite "test.db" (do
            catch (dropTable pTab1') (\(_::SomeException) -> return ())
            createTable pTab1'

            step1
            step2
            step3
            catch (dropTable pTab2') (\(_::SomeException) -> return ())
            createTable pTab2'
            insAuto pTab2 (map ($ ())
                [ ("один",).(1,).(2,)
                , ("два",).(2,).(3,)
                , ("три",).(3,).(1,)
                , ("ארבה",).(4,).(1,)
                , ("חמש",).(5,).(4,)
                ])
                >>= liftIO . print
        )
  where
    step1 = do
        ins pTab1 [ rec1
                  , rec2
                  , rec1 & lensIdName .~ (3,("odr",()))
                  , rec1 & lensIdName .~ (4,("elena",()))
                  , rec2 & lensIdName .~ ((5,).("text4",) $ ())
                  ]
        insAuto pTab1 [("text auto 1",).(Just 1.1,).(7,).("auto",).(Just "note",).r0 {-  -} $ ()]
        -- {-
        del pTab1 (Equal pId (1,()))
            >>= liftIO . print
        del pTab1 (Equal pId (1,()))
            >>= liftIO . print
        ins pTab1 [rec1]
        upd pTab1   [ rec1
                    & (recLens' (Proxy :: Proxy '(Plain,Rec1,'["name","_2"])))
                    .~ (("updated!",).(100500,) $ ())
                    ]
        -- -}
        insAuto pTab1 [("text auto 2",).(Just 2.1,).(10,).("test",).(Just "note2",).r0 {- -} $ ()]
            >>= liftIO . print
        sel pTab1 CondTrue >>= liftIO . mapM_ print
        return ()
    -- {-
    step2 = do
        sel pTab1 (Equal pIdName (rec1 ^. lensIdName)) >>= liftIO . mapM_ print
        sel pTab1 (Equal (pNRec pRec1) rec2) >>= liftIO . mapM_ print
        del pTab1 $ Equal pId (2,())
        sel pTab1 (Great pVal (Just 2,())) >>= liftIO . mapM_ print
        sel pTab1 (And [ Great pVal (Just 2,())
                       , Least pId  (7,())
                       ])
            >>= liftIO . mapM_ print
    -- -}
    -- {-
    step3 = do
        sel pTab1 (Null pVal) >>= liftIO . mapM_ print
        sel pTab1 (NotNull pVal) >>= liftIO . mapM_ print
        sel pTab1 (Not $ NotNull pVal) >>= liftIO . mapM_ print
        selProj (Proxy :: PTab1Sel '["id","val","z" ]) (Not $ NotNull pVal)
            >>= liftIO . mapM_ print
        sel pTab1 (Not $ NotNull pVal)
            >>= liftIO . mapM_ print
        return ()
    -- -}


-- TODO обработка ошибок
-- TODO внешние ключи
-- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом
-- TODO проверить добавление через def -- произвольный порядок полей
-- TODO транзакции
-- TODO: to make conduit (or pipe) for Select

type Tabs = '[Tab1, Tab2]

main :: IO ()
main = do
    sql
    run 8081 app

type MyAPI = PersAPI SimpleHtml Sqlite Tabs
myAPI = Proxy :: Proxy MyAPI

type DocsAPI = MyAPI :<|> "doc.md" :> Raw
api :: Proxy DocsAPI
api = Proxy

app = serve api serverD

runTestDB :: PersMonad Sqlite :~> EitherT ServantErr IO
runTestDB = Nat $ runSession sqlite "test.db"

serverD :: Server DocsAPI
serverD = server :<|> serveDocs
  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS
        plain = ("Content-Type", "text/plain")

server  = enter runTestDB
        $ persServerSimple (proxy# :: Proxy# Sqlite) (Proxy :: Proxy Tabs)

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] myAPI
  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]
