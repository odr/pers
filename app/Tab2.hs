{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}
module Tab2 where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol)
import Data.Int(Int64)
import qualified Data.Text as T
import Lens.Micro(Lens') -- (&), (.~), (^.))
import Control.Monad.Catch(catch, SomeException)
import Control.Monad.IO.Class(MonadIO(..))

import Pers.Types -- ((:::),Rep(Plain),VRec,recLens,pNRec,recLens')
import Pers.Database.DDL -- (TableDef, runSession, DDL(..))
import Pers.Database.DML -- (DML(..),Cond(..),InsAutoPK(..),sel)
import Pers.Database.Sqlite.Sqlite(Sqlite)
import Pers.Servant.Servant
import Pers.Servant.Simple

type Rec2 = '["id":::Int64,"name":::T.Text,"val":::Double,"tab1_id":::Int64]
type Tab2 = TableDef "tab2" Rec2 '["id"] '[ '["name"]]
                    '[ '["tab1_id":::"id"]:::("tab1":::Ref)]
pTab2 = Proxy :: Proxy '(Plain,Tab2)
pTab2' = Proxy :: Proxy Tab2

createTab2 :: SessionMonad Sqlite IO ()
createTab2 = do
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

type Tab2API = PersAPI' Plain SimpleHtml Sqlite '[ServData Plain Tab2]
serverTab2 = persServerSimple (proxy# :: Proxy# Plain)
                (proxy# :: Proxy# Sqlite) (Proxy  :: Proxy '[ServData Plain Tab2])
pTab2API :: Proxy Tab2API
pTab2API = Proxy
