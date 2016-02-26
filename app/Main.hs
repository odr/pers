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

import Pers.Types2 -- ((:::),Rep(Plain),VRec,recLens,pNRec,recLens')
import Pers.Database.DDL2 -- (TableDef, runSession, DDL(..))
import Pers.Database.DML2 -- (DML(..),Cond(..),InsAutoPK(..),sel)
import Pers.Database.Sqlite.Sqlite2(sqlite)

import Tab1

sql :: IO ()
sql = do
    runSession sqlite "test.db" (do
            catch (dropTable pTab1') (\(_::SomeException) -> return ())
            createTable pTab1'

            step1
            step2
            step3
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

main :: IO ()
main = do
    sql
