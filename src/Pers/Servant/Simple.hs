{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module Pers.Servant.Simple where
import Lucid
import Data.Int(Int64)
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Servant
import Servant.HTML.Lucid -- (HTML)
import Data.Aeson(ToJSON(..),FromJSON(..))
import qualified Data.Text as T

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML
import Pers.Servant.Servant

data SimpleHtml
{-
type Simple a = Typer SimpleHtml a -- (Proxy SimpleHtml, a) --

pSimple :: Proxy SimpleHtml
pSimple = Proxy

toSimple :: a -> Simple a
toSimple = pure -- (pSimple,)
-}

type Simple = FieldHtml SimpleHtml
toSimple :: a -> Simple a
toSimple = FieldHtml

persServerSimple :: PersServant SimpleHtml back x
    => Proxy# back -> Proxy (x::k)
    -> ServerT (PersAPI SimpleHtml back x) (PersMonad back)
persServerSimple = persServer (proxy# :: Proxy# SimpleHtml)


instance ToHtml (Simple Int64) where
    toHtml = toHtml . show . unFieldHtml
    toHtmlRaw = toHtml

instance ToHtml (Simple Double) where
    toHtml = toHtml . show . unFieldHtml
    toHtmlRaw = toHtml

instance ToHtml (Simple T.Text) where
    toHtml    = toHtml    . unFieldHtml
    toHtmlRaw = toHtmlRaw . unFieldHtml

instance (ToHtml (Simple a)) => ToHtml (Simple (Maybe a)) where
    toHtml    = maybe (toHtml "")    (toHtml    . toSimple) . unFieldHtml
    toHtmlRaw = maybe (toHtmlRaw "") (toHtmlRaw . toSimple) . unFieldHtml

-- HTML serialization of a single row
instance ToHtml (Simple ()) where
    toHtml    _ = return ()
    toHtmlRaw _ = return ()

instance (ToHtml (Simple v)) => ToHtml (Simple (v,())) -- no overloading with pair (Proxy,[x])
  where
    toHtml      = td_ . toHtml    . toSimple . fst . unFieldHtml
    toHtmlRaw   = td_ . toHtmlRaw . toSimple . fst . unFieldHtml

instance (ToHtml (Simple v1), ToHtml (Simple (v2,vs)))
        => ToHtml (Simple (v1,(v2,vs)))
  where
    toHtml    (FieldHtml (v,vs)) = td_ (toHtml    $ toSimple v) >> toHtml    (toSimple vs)
    toHtmlRaw (FieldHtml (v,vs)) = td_ (toHtmlRaw $ toSimple v) >> toHtmlRaw (toSimple vs)

-- HTML serialization of a list of rows
instance    ( ToHtml (Simple x)
            , Names (NRec a)
            , Rep rep a x
            )
            => ToHtml (Proxy '(rep,a), Simple [x])
  where
    toHtml (_,(FieldHtml xs))
        = table_ $ do
            tr_ $ foldMap (th_ . toHtml) $ names (proxy# :: Proxy# (NRec a))
            foldMap (tr_ . toHtml . toSimple) xs
    toHtmlRaw (_,(FieldHtml xs))
        = table_ $ do
            tr_ $ foldMap (th_ . toHtmlRaw) $ names (proxy# :: Proxy# (NRec a))
            foldMap (toHtmlRaw . toSimple) xs


instance    ( DBOption back
            , DML Plain back (TableDef n rec pk uk fk)
                            (VRec Plain rec)
                            (VRec Plain (ProjNames rec pk))
            , ContainNames rec (NRec rec)
            , Names (NRec rec)
            , RowRepDDL 'Plain back (ProjNames rec (NRec rec)) (VRec Plain rec)
            )
    => PersServant SimpleHtml back (TableDef n rec pk uk fk)
  where
    type PersAPI SimpleHtml back (TableDef n rec pk uk fk)
        = n :> "list" :> Get '[JSON,HTML] (Proxy '(Plain, rec), Simple [(VRec Plain rec)])
        -- :<|>

    persServer _ _ (_::Proxy (TableDef n rec pk uk fk)) = do
        fmap ((Proxy :: Proxy '(Plain, rec),) . toSimple)
            $ sel (Proxy :: Proxy '(Plain, TableDef n rec pk uk fk)) mempty

