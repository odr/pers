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
import Lens.Micro(Lens') -- (&), (.~), (^.))

import Pers.Types -- ((:::),Rep(Plain),VRec,recLens,pNRec,recLens')
import Pers.Database.DDL -- (TableDef, runSession, DDL(..))

type Rec1 = '["id":::Int64,"name":::T.Text,"val":::Maybe Double
             ,"x":::Int64, "z":::T.Text, "y"::: Maybe T.Text
             ,"_1":::Int64,"_2":::Int64,"_3":::Int64
             ,"_4":::Int64,"_5":::Int64,"_6":::Int64
             -- ,"7":::Int64,"8":::Int64,"9":::Int64
             -- ,"10":::Int64,"11":::Int64,"12":::Int64
             {-  -}
             ]
type Tab1 = TableDef "tab1" Rec1 '["id"]

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
