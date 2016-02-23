{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Catch

import Pers.Types((:::),Rep(Plain),VRec,recLens,pNRec)
import Pers.Database.DDL(TableDef, runSession, DDL(..))
import Pers.Database.DML(DML(..),Cond(..),InsAutoPK(..),sel)
import Pers.Database.Sqlite.Sqlite(sqlite)

type Rec1 = '["id":::Int64,"name":::T.Text,"val":::Maybe Double
             ,"x":::Int64, "z":::T.Text, "y"::: Maybe T.Text
             ,"_1":::Int64,"_2":::Int64,"_3":::Int64
             -- ,"4":::Int64,"5":::Int64,"6":::Int64
             -- ,"7":::Int64,"8":::Int64,"9":::Int64
             -- ,"10":::Int64,"11":::Int64,"12":::Int64
             {-  -}
             ]
type Tab1 = TableDef "tab1" Rec1 '["id"]

type PTab1Sel a = Proxy '(Plain, Tab1, a)
pRec1 = Proxy :: Proxy Rec1
pTab1 = Proxy :: Proxy '(Plain,Tab1)
pTab1' = Proxy :: Proxy Tab1

r0 = (1,).(2,).(3,) -- .(4,).(5,).(6,).(7,).(8,).(9,).(10,).(11,).(12,)
rec1 = (1,).("text1",).(Nothing,) .(4,).("ZZZ",).(Nothing,).r0 {-  -} $ ()
rec2 = (2,).("text2",).(Just 2.2,).(6,).("xxx",).(Nothing,).r0 {-  -} $ ()

type IdName = '["id":::Int64,"name":::T.Text]

lensIdName :: Lens' (VRec Plain Rec1) (VRec Plain IdName)
lensIdName = recLens (proxy# :: Proxy# '(Plain,Rec1,IdName))

-- type Name2 = '["name":::T.Text]
-- lensName2 :: Lens' (VRec Plain Rec1) (VRec Plain IdName)
-- lensName2 = recLens (proxy# :: Proxy# '(Plain,Rec1,IdName))

lensId = recLens (proxy# :: Proxy# '(Plain,Rec1,'["id":::Int64]))
pId = Proxy :: Proxy '["id":::Int64]
pVal = Proxy :: Proxy '["val":::Maybe Double]
pVal' = pNRec pVal
pIdName = Proxy :: Proxy '["id":::Int64,"name":::T.Text]

sql :: IO ()
sql = do
    runSession sqlite "test.db" (do
            catch (dropTable pTab1') (\(_::SomeException) -> return ())
            createTable pTab1'

            step1
            --step2
            --step3
        )
  where
    step1 = do
        ins pTab1 [ rec1
                  , rec2
                  , rec1 & lensId .~ (3,())
                  , rec1 & lensId .~ (4,())
                  , rec2 & lensIdName .~ ((5,).("text4",) $ ())
                  ]
        insAuto pTab1 [("text auto 1",).(Just 1.1,).(7,).("auto",).(Just "note",).r0 {-  -} $ ()]
        del pTab1 (Equal pId (1,()))
            >>= liftIO . print
        del pTab1 (Equal pId (1,()))
            >>= liftIO . print
        ins pTab1 [rec1]
        upd pTab1   [ rec1
                    & (recLens (proxy# :: Proxy# '(Plain,Rec1,'["name":::T.Text,"_2":::Int64])))
                    .~ (("updated!",).(100500,) $ ())
                    ]
        insAuto pTab1 [("text auto 2",).(Just 2.1,).(10,).("test",).(Just "note2",).r0 {- -} $ ()]
            >>= liftIO . print
        sel pTab1 CondTrue >>= liftIO . mapM_ print
        return ()
    {-
    step2 = do
        sel pTab1 (Equal pIdName (rec1 ^. lensIdName)) >>= liftIO . mapM_ print
        sel pTab1 (Equal pRec1 rec2) >>= liftIO . mapM_ print
        del pTab1 $ Equal pId (2,())
        sel pTab1 (Great pVal (Just 2,())) >>= liftIO . mapM_ print
        sel pTab1 (And [ Great pVal (Just 2,())
                       , Least pId  (7,())
                       ])
            >>= liftIO . mapM_ print
    step3 = do
        sel pTab1 (Null pVal') >>= liftIO . mapM_ print
        sel pTab1 (NotNull pVal') >>= liftIO . mapM_ print
        selProj (Proxy :: PTab1Sel '["id","val","z" {-  -} ]) (Not $ NotNull pVal')
            >>= liftIO . mapM_ print
        sel pTab1 (Not $ NotNull pVal')
            >>= liftIO . mapM_ print
    -}


-- TODO обработка ошибок
-- TODO внешние ключи
-- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом
-- TODO проверить добавление через def -- произвольный порядок полей
-- TODO транзакции
-- TODO: to make conduit (or pipe) for Select

main :: IO ()
main = do
    sql
