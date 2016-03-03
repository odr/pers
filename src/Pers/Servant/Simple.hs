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
import Data.Tagged

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML
import Pers.Servant.Servant

data SimpleHtml

type Simple = Tagged SimpleHtml
toSimple :: a -> Simple a
toSimple = Tagged

persServerSimple :: PersServant SimpleHtml back x
    => Proxy# back -> Proxy (x::k)
    -> ServerT (PersAPI SimpleHtml back x) (PersMonad back)
persServerSimple = persServer (proxy# :: Proxy# SimpleHtml)


instance ToHtml (Simple Int64) where
    toHtml = toHtml . show . untag
    toHtmlRaw = toHtml

instance ToHtml (Simple Double) where
    toHtml = toHtml . show . untag
    toHtmlRaw = toHtml

instance ToHtml (Simple T.Text) where
    toHtml    = toHtml    . untag
    toHtmlRaw = toHtmlRaw . untag

instance (ToHtml (Simple a)) => ToHtml (Simple (Maybe a)) where
    toHtml    = maybe (toHtml "")    (toHtml    . toSimple) . untag
    toHtmlRaw = maybe (toHtmlRaw "") (toHtmlRaw . toSimple) . untag

-- HTML serialization of a single row
instance ToHtml (Simple ()) where
    toHtml    _ = return ()
    toHtmlRaw _ = return ()

instance (ToHtml (Simple v)) => ToHtml (Simple (v,())) -- no overloading with pair (Proxy,[x])
  where
    toHtml      = td_ . toHtml    . toSimple . fst . untag
    toHtmlRaw   = td_ . toHtmlRaw . toSimple . fst . untag

instance (ToHtml (Simple v1), ToHtml (Simple (v2,vs)))
        => ToHtml (Simple (v1,(v2,vs)))
  where
    toHtml    (Tagged (v,vs)) = td_ (toHtml    $ toSimple v) >> toHtml    (toSimple vs)
    toHtmlRaw (Tagged (v,vs)) = td_ (toHtmlRaw $ toSimple v) >> toHtmlRaw (toSimple vs)

-- HTML serialization of a list of rows
instance    ( ToHtml (Simple x)
            , Names (NRec a)
            , Rep rep a x
            )
            => ToHtml (Proxy '(rep,a), Simple [x])
  where
    toHtml (_,(Tagged xs))
        = table_ $ do
            tr_ $ foldMap (th_ . toHtml) $ names (proxy# :: Proxy# (NRec a))
            foldMap (tr_ . toHtml . toSimple) xs
    toHtmlRaw (_,(Tagged xs))
        = table_ $ do
            tr_ $ foldMap (th_ . toHtmlRaw) $ names (proxy# :: Proxy# (NRec a))
            foldMap (tr_ . toHtmlRaw . toSimple) xs



