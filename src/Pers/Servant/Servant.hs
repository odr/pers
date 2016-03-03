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
module Pers.Servant.Servant where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Servant
import Control.Monad.Trans.Either
import Data.Aeson(ToJSON(..),FromJSON(..))
import Servant.HTML.Lucid(HTML)
import Data.Tagged

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML

type PersMonad back = SessionMonad back (EitherT ServantErr IO)

-- newtype FieldHtml r a = FieldHtml { unFieldHtml :: a }

instance (ToJSON x) => ToJSON (Tagged r x) where
    toJSON = toJSON . untag
instance (FromJSON x) => FromJSON (Tagged r x) where
    parseJSON = fmap Tagged . parseJSON

instance (ToJSON (Proxy x, y)) => ToJSON (Proxy x, Tagged r y) where
    toJSON = toJSON . fmap untag

instance (FromJSON (Proxy x, y)) => FromJSON (Proxy x, Tagged r y) where
    parseJSON = fmap (fmap Tagged) . parseJSON

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

instance    ( DBOption back
            , DML Plain back (TableDef n rec pk uk fk)
                            (VRec Plain rec)
                            (VRec Plain (ProjNames rec pk))
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
            "list" :> Get '[HTML,JSON]
                (Proxy '(Plain, rec), Tagged opt [VRec Plain rec])
            :<|>
            "ins" :> ReqBody '[JSON] [VRec Plain rec] :> Post '[JSON] ()
            :<|>
        -- "insAuto"  :> ReqBody '[JSON] [VRec Plain (DataRecord rec)]
        --                 :> Post '[JSON] (VRec Plain (Key rec))
        -- :<|>
            "upd"  :> ReqBody '[JSON] [VRec Plain rec]
                    :> Put '[JSON] [VRec Plain (ProjNames rec pk)]
            :<|>
            "del" :> ReqBody '[JSON] [VRec Plain (ProjNames rec pk)] :> Delete '[JSON] (Tagged "del" Int)
            )
    persServer _ _ (_::Proxy (TableDef n rec pk uk fk))
        =    fmap ((pRec,) . Tagged) (sel pTab mempty)
        :<|> ins pTab
        -- :<|> insAuto pTab
        :<|> upd pTab
        :<|> fmap (Tagged . sum) . mapM (del pTab . Equal pPk)
      where
        pRec = Proxy :: Proxy '(Plain, rec)
        pTab = Proxy :: Proxy '(Plain, TableDef n rec pk uk fk)
        pPk  = Proxy :: Proxy pk

