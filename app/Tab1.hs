{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE PolyKinds #-}
module Tab1 where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.TypeLits(Symbol)
import Data.Int(Int64)
import qualified Data.Text as T
import Lens.Micro(Lens', (&), (.~), (^.))
import Control.Monad.Catch(catch, SomeException)
import Control.Monad.IO.Class(MonadIO(..))

import Pers.Types -- ((:::),Rep(Plain),VRec,recLens,pNRec,recLens')
import Pers.Database.DDL -- (TableDef, runSession, DDL(..))
import Pers.Database.DML -- (DML(..),Cond(..),InsAutoPK(..),sel)
import Pers.Database.Sqlite.Sqlite(Sqlite)
import Pers.Servant.Servant
import Pers.Servant.Simple

type Rec1 = '["id":::Int64,"name":::T.Text,"val":::Maybe Double
             ,"x":::Int64, "z":::T.Text, "y"::: Maybe T.Text
             ,"_1":::Int64,"_2":::Int64,"_3":::Int64
             ,"_4":::Int64,"_5":::Int64,"_6":::Int64
             -- ,"7":::Int64,"8":::Int64,"9":::Int64
             -- ,"10":::Int64,"11":::Int64,"12":::Int64
             {-  -}
             ]
type Tab1 = TableDef "tab1" Rec1 '["id"] '[ '["name"]] '[]

type PTab1Sel (a::[Symbol]) = Proxy '(Plain, Tab1, a)
pRec1 = Proxy :: Proxy Rec1
pTab1 = Proxy :: Proxy '(Plain,Tab1)
pTab1' = Proxy :: Proxy Tab1

r0 = (1,).(2,).(3,).(4,).(5,).(6,) -- .(7,).(8,).(9,).(10,).(11,).(12,)
rec1,rec2 :: VRec Plain Rec1
rec1 = (1,).("text1",).(Nothing,) .(4,).("ZZZ",).(Nothing,).r0 {-  -} $ ()
rec2 = (2,).("text2",).(Just 2.2,).(6,).("xxx",).(Nothing,).r0 {-  -} $ ()

type IdName = '["id":::Int64,"name":::T.Text]

lensIdName :: Lens' (VRec Plain Rec1) (VRec Plain IdName)
-- lensIdName :: (RecLens Plain Rec1 IdName br ar) => Lens' br ar
lensIdName = recLens (proxy# :: Proxy# '(Plain,Rec1,IdName))

-- type Name2 = '["name":::T.Text]
-- lensName2 :: Lens' (VRec Plain Rec1) (VRec Plain IdName)
-- lensName2 = recLens (proxy# :: Proxy# '(Plain,Rec1,IdName))

lensId :: Lens' (VRec Plain Rec1) (VRec Plain '["id":::Int64])
-- lensId :: (RecLens Plain Rec1 '["id":::Int64] br ar) => Lens' br ar
lensId = recLens (proxy# :: Proxy# '(Plain,Rec1,'["id":::Int64]))

pId = Proxy :: Proxy '["id"]
pVal = Proxy :: Proxy '["val"]
pIdName = Proxy :: Proxy '["id","name"]

createTab1 :: SessionMonad Sqlite IO ()
createTab1 = do
    catch (dropTable pTab1') (\(_::SomeException) -> return ())
    createTable pTab1'

    step1
    step2
    step3
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

type Tab1API = PersAPI' Plain SimpleHtml Sqlite Tab1
serverTab1 = persServerSimple (proxy# :: Proxy# Plain)
                (proxy# :: Proxy# Sqlite) (Proxy  :: Proxy (ServData Plain Tab1))

pTab1API :: Proxy Tab1API
pTab1API = Proxy
pTab1API' :: Proxy '(Plain,SimpleHtml,Sqlite,ServData Plain Tab1)
pTab1API' = Proxy
