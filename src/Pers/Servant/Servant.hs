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

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML

type PersMonad back = SessionMonad back (EitherT ServantErr IO)

class PersServant opt back (x::k) where
    type PersAPI   opt back x
    persServer :: Proxy# opt -> Proxy# back -> Proxy x
        -> ServerT (PersAPI opt back x) (PersMonad back)

instance (PersServant opt back x) => PersServant opt back '[x] where
    type PersAPI opt back '[x]      = PersAPI opt back x
    persServer po pb _ = persServer po pb (Proxy :: Proxy x)

instance (PersServant opt back x1, PersServant opt back (x2 ': xs))
        => PersServant opt back (x1 ': x2 ': xs)
  where
    type PersAPI opt back (x1 ': x2 ': xs)
        =   PersAPI opt back x1
        :<|> PersAPI opt back (x2 ': xs)
    persServer po pb (_::Proxy (x1 ': x2 ': xs))
        =    persServer po pb (Proxy :: Proxy x1)
        :<|> persServer po pb (Proxy :: Proxy (x2 ': xs))

type PlainRecs rec = (Proxy '(Plain, rec), [VRec Plain rec])
type PlainPks rec pk = (Proxy '(Plain, ProjNames rec pk), [VRec Plain (ProjNames rec pk)])

instance    ( DBOption back
            , DML Plain back (TableDef n rec pk uk fk)
                            (VRec Plain rec)
                            (VRec Plain (ProjNames rec pk))
                            (VRec Plain (MinusNames rec pk))
            , ContainNames rec (NRec rec)
            , ContainNames rec pk
            , Names (NRec rec)
            , Names pk
            , RowRepDDL 'Plain back (ProjNames rec (NRec rec)) (VRec Plain rec)
            , RowRepDDL 'Plain back (ProjNames rec pk) (VRec 'Plain (ProjNames rec pk))
            )
    => PersServant opt back (TableDef n rec pk uk fk)
  where
    type PersAPI opt back (TableDef n rec pk uk fk) =
        n :> (
            Get '[HTML,JSON] (Tagged opt (PlainRecs rec))
            :<|>
            ReqBody '[JSON] (PlainRecs rec) :> Post '[JSON] ()
            :<|>
        -- "insAuto"  :> ReqBody '[JSON] [VRec Plain (DataRecord rec)]
        --                 :> Post '[JSON] (VRec Plain (Key rec))
        -- :<|>
            ReqBody '[JSON] (PlainRecs rec)
                    :> Put '[JSON] (PlainPks rec pk)
            :<|>
            ReqBody '[JSON] (PlainPks rec pk)
                   :> Delete '[JSON] Int
            )
    persServer _ _ (_::Proxy (TableDef n rec pk uk fk)) =
        fmap (Tagged . (pRec,)) (sel pTab mempty)
        :<|>
        ins pTab . snd
        -- :<|>
        -- insAuto pTab
        :<|>
        fmap (pPk,) . upd pTab . snd
        :<|>
        fmap sum . mapM (del pTab . Equal pPkN) . snd
      where
        pRec = Proxy :: Proxy '(Plain, rec)
        pTab = Proxy :: Proxy '(Plain, TableDef n rec pk uk fk)
        pPkN  = Proxy :: Proxy pk
        pPk  = Proxy :: Proxy '(Plain, ProjNames rec pk)

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

-- instance (ToSample r r) => ToSample (Proxy '(Plain, xs), r) r
--   where
--     toSample _ = toSample (Proxy :: Proxy r)
--
{-
instance (ToSample v v', ToJSON v', KnownSymbol n)
    => ToSample (Proxy '(Plain, '[ '(n,v)]), (v,()))
                (Tagged (Proxy '(Plain, '[ '(n,v)]), (v,())) (Text,BL.ByteString))
  where
    toSample _ = Just $ Tagged (s,sv)
      where
        s = T.pack $ symbolVal (Proxy :: Proxy n)
        sv = encode $ toJSON $ toSample (Proxy :: Proxy v)

instance (Rep Plain ( x ': x1 ': xs) r
        , Rep Plain (x1 ': xs) rs
        , ToSample (Proxy '(Plain, x), r)
                    (Tagged (Proxy '(Plain, x), r) (Text,BL.ByteString))
        , ToSample (Proxy '(Plain, x1 ': xs), rs)
                    (Tagged (Proxy '(Plain, x1 ': xs), rs) [(Text,BL.ByteString)])
        )
        => ToSample (Proxy '(Plain, ( x ': x1 ': xs)), r)
                    (Tagged (Proxy '(Plain, ( x ': x1 ': xs)), r)
                            [(Text,BL.ByteString)])
  where
    toSample _  = fmap Tagged $ (:) <$> sv <*> sxs
      where
        sv  = fmap untag $ toSample (Proxy :: Proxy (Proxy '(Plain, x), r))
        sxs = fmap untag $ toSample (Proxy :: Proxy (Proxy '(Plain, x1 ': xs), rs))

instance (ToSample (Proxy '(Plain, xs), r)
                    (Tagged (Proxy '(Plain, xs), r) [(Text,BL.ByteString)])
        , Rep Plain xs r
        )
        => ToSample (Proxy '(Plain, xs), [r])
                    (Tagged (Proxy '(Plain, xs), [r]) [(Text,BL.ByteString)])
  where
    toSample _ = fmap retag $ toSample (Proxy :: Proxy (Proxy '(Plain, xs), r))
-}
