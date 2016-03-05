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

type PRecs rep rec ar = (Proxy '(rep, rec), [ar])
type PPks rep rec pk kr = (Proxy '(rep, ProjNames rec pk), [kr])
-- type PDrs rep (rec::[(Symbol,*)]) pk
--     = (Proxy '(Plain, MinusNames rec pk), [VRec Plain (MinusNames rec pk)])

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
            -- , RowRepDDL rep back (MinusNames rec pk) dr
            )
    => PersServant (rep::R) opt back (TableDef n rec pk uk fk) ar kr dr
  where
    type PersAPI rep opt back (TableDef n rec pk uk fk) ar kr dr =
        n :> (
            Get '[HTML,JSON] (Tagged opt (PRecs rep rec ar))
            :<|>
            -- ReqBody '[JSON] (PRecs rep rec ar) :> Post '[JSON] ()
            PersInsAPI (IsAutoPKb rep back kr) rep back
                        (TableDef n rec pk uk fk) ar kr dr
            :<|>
            ReqBody '[JSON] (PRecs rep rec ar)
                    :> Put '[JSON] (PPks rep rec pk kr)
            :<|>
            ReqBody '[JSON] (PPks rep rec pk kr)
                   :> Delete '[JSON] Int
            )
    persServer pr _ pb pa = -- (_::Proxy (TableDef n rec pk uk fk)) =
        fmap (Tagged . (pRec,)) (sel pTab mempty)
        -- :<|>
        -- (case eqT :: Maybe (IsAutoPKb Plain back
        --                     (VRec Plain (ProjNames rec pk)) :~: True) of
        --     Nothing   -> ins pTab . snd
        --     Just Refl -> insAuto pTab
        -- )
        :<|>
        -- ins pTab . snd
        persInsServer (proxy# :: Proxy# (IsAutoPKb rep back kr)) pr pb pa
        -- :<|>
        -- insAuto pTab
        :<|>
        fmap (pPk,) . upd pTab . snd
        :<|>
        fmap sum . mapM (del pTab . Equal pPkN) . snd
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

instance (DML rep back a ar kr dr, IsAutoPK rep back kr)
        => PersServantIns True rep back a ar kr dr
  where
    type PersInsAPI True rep back a ar kr dr
        = ReqBody '[JSON] (PRecs rep (DataRecord a) dr) :> Post '[JSON] [kr]
    persInsServer _ _ _ _ = insAuto pTab . snd
      where
        pTab = Proxy :: Proxy '(rep, a)

instance (DML rep back a ar kr dr)
        => PersServantIns False rep back a ar kr dr
  where
    type PersInsAPI False rep back a ar kr dr
        = ReqBody '[JSON] (PRecs rep (RecordDef a) ar) :> Post '[JSON] ()
    persInsServer _ _ _ _ = ins pTab . snd
      where
        pTab = Proxy :: Proxy '(rep, a)

--------- ServantDoc ---------------

instance (ToSample x x) => ToSample (Tagged r x) (Tagged r x) where
    toSample _ = fmap Tagged $ toSample (Proxy :: Proxy x)

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
    => ToSample (Proxy '(Plain, '(n,v)), v)
                (Proxy '(Plain, '(n,v)), v')
  where
    toSample _  = fmap (Proxy :: Proxy '(Plain, '(n,v)),)
                $ toSample (Proxy :: Proxy v)

instance (ToSample v v')
    => ToSample (Proxy '(Plain, '[ '(n,v)]), (v,()))
                (Proxy '(Plain, '[ '(n,v)]), (v',()))
  where
    toSample _  = fmap ((Proxy :: Proxy '(Plain, '[ '(n,v)]),) . (,()))
                $ toSample (Proxy :: Proxy v)

instance (Rep Plain (x1 ': xs) rs
        , Rep Plain '[x] (r,())
        , ToSample (Proxy '(Plain, x), r) (Proxy '(Plain, x), r')
        , ToSample (Proxy '(Plain, x1 ': xs), rs) (Proxy '(Plain, x1 ': xs), rs')
        )
        => ToSample (Proxy '(Plain, ( x ': x1 ': xs)), (r,rs))
                    (Proxy '(Plain, ( x ': x1 ': xs)), (r',rs'))
  where
    toSample _  = fmap (Proxy :: Proxy '(Plain, ( x ': x1 ': xs)),)
                $ (,) <$> sv <*> sxs
      where
        sv  = fmap snd $ toSample (Proxy :: Proxy (Proxy '(Plain, x), r))
        sxs = fmap snd $ toSample (Proxy :: Proxy (Proxy '(Plain, x1 ': xs), rs))

instance (ToSample (Proxy '(Plain, xs), r) (Proxy '(Plain, xs), r')
        , Rep Plain xs r
        )
        => ToSample (Proxy '(Plain, xs), [r]) (Proxy '(Plain, xs), [r'])
  where
    toSample _ = fmap (fmap (:[]))
            $ toSample (Proxy :: Proxy (Proxy '(Plain, xs), r))

