{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Pers.Servant.Servant where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Servant
import Control.Monad.Trans.Either
-- import Control.Monad.Except
import Data.Aeson(ToJSON(..),FromJSON(..), encode)
import Servant.HTML.Lucid(HTML)
import Servant.Docs
import Data.Tagged
import Data.Text(Text)
import qualified Data.Text as T
import Data.Int(Int64)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import GHC.TypeLits(Symbol(..), KnownSymbol, SomeSymbol(..), symbolVal', symbolVal)
import Data.Type.Bool
import Data.Typeable

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML

-- type PersMonad back = SessionMonad back (ExceptT ServantErr IO)
type PersMonad back = SessionMonad back (EitherT ServantErr IO)

type ServData rep x
    = '(x, VRec rep (RecordDef x), VRec rep (Key x), VRec rep (DataRecord x))
type family ServDatas rep xs where
    ServDatas rep '[] = '[]
    ServDatas rep (x ': xs) = ServData rep x ': ServDatas rep xs

class   ( TableLike a
        , Rep rep (RecordDef a) ar
        , Rep rep (Key a) kr
        , Rep rep (DataRecord a) dr
        )
        => PersServant (rep::R) opt back a ar kr dr
                | rep a -> ar, rep a -> kr, rep a -> dr where
    type PersAPI rep opt back a ar kr dr
    persServer :: Proxy# rep -> Proxy# opt -> Proxy# back -> Proxy '(a,ar,kr,dr)
        -> ServerT (PersAPI rep opt back a ar kr dr) (PersMonad back)

class PersServant' (rep::R) opt back (x::[(DataDef *,*,*,*)])
  where
    type PersAPI' rep opt back x
    persServer' :: Proxy# rep -> Proxy# opt -> Proxy# back -> Proxy x
        -> ServerT (PersAPI' rep opt back x) (PersMonad back)

instance (PersServant rep opt back a ar kr dr)
        => PersServant' rep opt back '[ '(a,ar,kr,dr)]
  where
    type PersAPI' rep opt back '[ '(a,ar,kr,dr)]
        = PersAPI rep opt back a ar kr dr
    persServer' pr po pb _
        = persServer pr po pb (Proxy :: Proxy '(a,ar,kr,dr))

instance    ( PersServant' rep opt back '[a]
            , PersServant' rep opt back (a1 ': as)
            )
            => PersServant' rep opt back (a ': a1 ': as) where
    type PersAPI' rep opt back (a ': a1 ': as)
        = PersAPI' rep opt back '[a]
        :<|> PersAPI' rep opt back (a1 ': as)
    persServer' pr po pb (_::Proxy (a ': a1 ': as))
        = persServer' pr po pb (Proxy :: Proxy '[a])
        :<|> persServer' pr po pb (Proxy :: Proxy (a1 ': as))

type PRecs rep rec ar = Tagged ('(rep, rec)) [ar]
type PPks rep rec pk kr = Tagged ('(rep, ProjNames rec pk)) [kr]

instance    ( DBOption back
            , DML rep back (TableDef n rec pk uk fk) ar kr dr
            , ContainNames rec (NRec rec)
            , ContainNames rec pk
            , Names (NRec rec)
            , Names pk
            , RowRepDDL rep back (ProjNames rec (NRec rec)) ar
            , RowRepDDL rep back (ProjNames rec pk) kr
            , PersServantIns (IsAutoPKb rep back kr) rep back
                             (TableDef n rec pk uk fk) ar kr dr
            )
    => PersServant (rep::R) opt back (TableDef n rec pk uk fk) ar kr dr
  where
    type PersAPI rep opt back (TableDef n rec pk uk fk) ar kr dr =
        n :> (
            Get '[HTML,JSON] (Tagged opt (PRecs rep rec ar))
            :<|>
            PersInsAPI (IsAutoPKb rep back kr) rep back
                        (TableDef n rec pk uk fk) ar kr dr
            :<|>
            ReqBody '[JSON] (PRecs rep rec ar)
                    :> Put '[JSON] (PPks rep rec pk kr)
            :<|>
            ReqBody '[JSON] (PPks rep rec pk kr)
                   :> Delete '[JSON] Int
            )
    persServer pr _ pb pa =
        fmap (Tagged . Tagged) (sel pTab mempty)
        :<|>
        persInsServer (proxy# :: Proxy# (IsAutoPKb rep back kr)) pr pb pa
        :<|>
        fmap Tagged . upd pTab . untag
        :<|>
        fmap sum . mapM (del pTab . Equal pPkN) . untag
      where
        pRec = Proxy :: Proxy '(rep, rec)
        pTab = Proxy :: Proxy '(rep, TableDef n rec pk uk fk)
        pPkN  = Proxy :: Proxy pk
        pPk  = Proxy :: Proxy '(rep, ProjNames rec pk)


-- local class
class   ( TableLike a
        , Rep rep (RecordDef a) ar
        , Rep rep (Key a) kr
        , Rep rep (DataRecord a) dr
        )
        => PersServantIns (isAuto::Bool) (rep::R) (back:: *) (a::DataDef *) ar kr dr
                | rep a -> ar, rep a -> kr, rep a -> dr where
    type PersInsAPI isAuto rep back a ar kr dr
    persInsServer :: Proxy# isAuto -> Proxy# rep -> Proxy# back -> Proxy '(a,ar,kr,dr)
        -> ServerT (PersInsAPI isAuto rep back a ar kr dr) (PersMonad back)

instance (DML rep back a ar kr dr, IsAutoPK rep back kr, TableLike a)
        => PersServantIns True rep back a ar kr dr
  where
    type PersInsAPI True rep back a ar kr dr
        = ReqBody '[JSON] (PRecs rep (DataRecord a) dr)
        :> Post '[JSON] (Tagged '(rep,Key a) [kr])
    persInsServer _ _ _ _ = fmap Tagged . insAuto pTab . untag
      where
        pTab = Proxy :: Proxy '(rep, a)

instance (DML rep back a ar kr dr, TableLike a)
        => PersServantIns False rep back a ar kr dr
  where
    type PersInsAPI False rep back a ar kr dr
        = ReqBody '[JSON] (PRecs rep (RecordDef a) ar)
        :> Post '[JSON] ()
    persInsServer _ _ _ _ = ins pTab . untag
      where
        pTab = Proxy :: Proxy '(rep, a)

--------- ServantDoc ---------------
{-
instance (ToSample x) => ToSample (Tagged r x) where
    toSamples _ = map (fmap Tagged) $ toSamples (Proxy :: Proxy x)
instance ToSample Int where
    toSamples _ = [("val",1)]
instance ToSample Int64 where
    toSamples _ = [("val",64)]
instance ToSample Double where
    toSamples _ = [("val",1.2)]
instance ToSample Text where
    toSamples _ = [("E","English"), ("R","Русский"), ("H","עברית"), ("J","日本の")]
instance ToSample ByteString where
    toSamples _ = [("ByteString", "Some long text")]
-}
-- {-
--instance (ToSample x x) => ToSample (Tagged r x) (Tagged r x) where
--    toSample _ = fmap Tagged $ toSample (Proxy :: Proxy x)

-- Надо делать спец типы и их для разных БД адаптировать и для документации
instance ToSample () () where
    toSample _ = Nothing
instance ToSample Int64 Int64 where
    toSample _ = Just 11
instance ToSample Int Int where
    toSample _ = Just 1
instance ToSample [Int64] [Int64] where
    toSample _ = Just [11,21]
instance ToSample [(Int64,())] [(Int64,())] where
    toSample _ = Just [(11,()),(21,())]
instance ToSample Double Double where
    toSample _ = Just 1.2
instance ToSample [Double] [Double] where
    toSample _ = Just [1.2,2.2]
instance ToSample Text Text where
    toSample _ = Just "Some text"
instance ToSample [Text] [Text] where
    toSample _ = Just ["English", "Русский", "עברית", "日本の"]
instance ToSample ByteString ByteString where
    toSample _ = Just "Some long text"
instance ToSample [ByteString] [ByteString] where
    toSample _ = Just ["Some long text", "More and more..."]
instance (ToSample a b) => ToSample (Maybe a) (Maybe b) where
    toSample _ = Just $ toSample (Proxy :: Proxy a)

instance (ToSample v v')
    => ToSample (Tagged '(Plain, '(n,v)) v)
                (Tagged '(Plain, '(n,v)) v')
  where
    toSample _  = fmap Tagged $ toSample (Proxy :: Proxy v)

instance (ToSample v v')
    => ToSample (Tagged '(Plain, '[ '(n,v)]) (v ,()))
                (Tagged '(Plain, '[ '(n,v)]) (v',()))
  where
    toSample _  = fmap (Tagged . (,())) $ toSample (Proxy :: Proxy v)

instance (Rep Plain (x1 ': xs) rs
        , Rep Plain '[x] (r,())
        , ToSample (Tagged '(Plain, x) r) (Tagged '(Plain, x) r')
        , ToSample (Tagged '(Plain, x1 ': xs) rs) (Tagged '(Plain, x1 ': xs) rs')
        )
        => ToSample (Tagged '(Plain, ( x ': x1 ': xs)) (r ,rs ))
                    (Tagged '(Plain, ( x ': x1 ': xs)) (r',rs'))
  where
    toSample _  = fmap Tagged $ (,) <$> sv <*> sxs
      where
        sv  = fmap untag $ toSample (Proxy :: Proxy (Tagged '(Plain, x) r))
        sxs = fmap untag $ toSample (Proxy :: Proxy (Tagged '(Plain, x1 ': xs) rs))

instance (ToSample (Tagged '(Plain, xs) r) (Tagged '(Plain, xs) r')
        , Rep Plain xs r
        )
        => ToSample (Tagged '(Plain, xs) [r]) (Tagged '(Plain, xs) [r'])
  where
    toSample _ = fmap (fmap (:[]))
            $ toSample (Proxy :: Proxy (Tagged '(Plain, xs) r))
-- -}
