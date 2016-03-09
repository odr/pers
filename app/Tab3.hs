{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}
module Tab3 where

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

type Rec3 = ["t1_id":::Int64,"t2_id":::Int64]
type Tab3 = TableDef "tab3" Rec3 '["t1_id","t2_id"] '[]
       '[ '["t1_id":::"id"]:::("tab1":::Parent)
        , '["t2_id":::"id"]:::("tab2":::Parent)
        ]
pTab3 = Proxy :: Proxy '(Plain,Tab3)
pTab3' = Proxy :: Proxy Tab3

createTab3 :: SessionMonad Sqlite IO ()
createTab3 = do
    catch (dropTable pTab3') (\(_::SomeException) -> return ())
    createTable pTab3'
    ins pTab3 $ map ($ ()) [(1,).(1,), (2,).(1,)]

type Tab3API = PersAPI' Plain SimpleHtml Sqlite '[ServData Plain Tab3]
serverTab3 = persServerSimple (proxy# :: Proxy# Plain)
                (proxy# :: Proxy# Sqlite) (Proxy  :: Proxy '[ServData Plain Tab3])
