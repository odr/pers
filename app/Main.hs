{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

-- import Data.Default(Default(..))
-- import qualified Data.Map as M
-- import Database.Persist(toPersistValue)
import Lens.Micro
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol)
import Data.Int(Int64)
import GHC.TypeLits(SomeSymbol, someSymbolVal)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
-- import Control.Monad.Trans.Reader(runReaderT)
import Control.Monad.IO.Class(MonadIO(..))
-- import Control.Monad.Catch
-- import Database.SQLite3
import qualified Data.Text as T
-- import Lens.Micro

import Pers.Types

import Pers.Database.DDL
import Pers.Database.DML
import Pers.Database.Sqlite.Sqlite

type Rec1 = '["id":::Int64,"name":::T.Text,"val":::Maybe Double
             ,"x":::Int64, "z":::T.Text {-  -}
             ]
type Tab1 = TableDef "tab1" Rec1 '["id"]

type PTab1Sel a = Proxy '(Plain, Tab1, a)
pRec1 = Proxy :: Proxy Rec1
pTab1 = Proxy :: Proxy '(Plain,Tab1)
pTab1' = Proxy :: Proxy Tab1

rec1 = (1,).("text1",).(Nothing,) .(4,).("ZZZ",){-  -} $ ()
rec2 = (2,).("text2",).(Just 2.2,).(6,).("xxx",){-  -} $ ()

type IdName = '["id":::Int64,"name":::T.Text]
lensIdName = recLens (proxy# :: Proxy# '(Plain,Rec1,IdName))
lensId = recLens (proxy# :: Proxy# '(Plain,Rec1,'["id":::Int64]))
pId = Proxy :: Proxy '["id":::Int64]
pVal = Proxy :: Proxy '["val":::Maybe Double]
pVal' = pNRec pVal
pIdName = Proxy :: Proxy '["id":::Int64,"name":::T.Text]

sql :: IO ()
sql = do
    runSession sqlite "test.db" (do
            dropTable pTab1'
            createTable pTab1'

            ins pTab1 [ rec1
                      , rec2
                      , rec1 & lensId .~ (3,())
                      , rec1 & lensId .~ (4,())
                      , rec2 & lensIdName .~ ((5,).("text4",) $ ())
                      ]
            insAuto pTab1 [("text auto 1",).(Just 1.1,).(7,).("auto",) {-  -} $ ()]
            return ()

            del pTab1 (Equal pId (1,()))
                >>= liftIO . print
            del pTab1 (Equal pId (1,()))
                >>= liftIO . print

            ins pTab1 [rec1]
            insAuto pTab1 [("text auto 2",).(Just 2.1,).(10,).("test",)  {- -} $ ()]
                >>= liftIO . print

            sel pTab1 CondTrue >>= liftIO . mapM_ print
            sel pTab1 (Equal pIdName (rec1 ^. lensIdName))
                >>= liftIO . mapM_ print
            sel pTab1 (Equal pRec1 rec2)
                >>= liftIO . mapM_ print
            del pTab1 $ Equal pId (2,())

            sel pTab1 (Great pVal (Just 2,()))
                >>= liftIO . mapM_ print
            sel pTab1 (And [ Great pVal (Just 2,())
                           , Least pId  (7,())
                           ])
                >>= liftIO . mapM_ print

            sel pTab1 (Null pVal')
                >>= liftIO . mapM_ print
            sel pTab1 (NotNull pVal')
                >>= liftIO . mapM_ print
            selProj (Proxy :: PTab1Sel '["id","val","z" {-  -} ]) (Not $ NotNull pVal')
                >>= liftIO . mapM_ print
            sel pTab1 (Not $ NotNull pVal')
                >>= liftIO . mapM_ print

            -- TODO обработка ошибок
            -- TODO внешние ключи
            -- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом
            -- TODO проверить добавление через def -- произвольный порядок полей
            -- TODO транзакции
            -- TODO: to make conduit (or pipe) for Select

        )

main :: IO ()
main = do
    sql
