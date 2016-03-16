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
{-# LANGUAGE OverloadedStrings #-}
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
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal')
import Lens.Micro
import Servant.Docs

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML
import Pers.Servant.Servant
import Pers.Servant.Input

data SimpleHtml

type Simple = Tagged SimpleHtml
toSimple :: a -> Simple a
toSimple = Tagged

persServerSimple :: PersServant' rep SimpleHtml back x
    => Proxy# rep -> Proxy# back -> Proxy (x::[(DataDef *,*,*,*)])
    -> ServerT (PersAPI' rep SimpleHtml back x) (PersMonad back)
persServerSimple pr = persServer' pr (proxy# :: Proxy# SimpleHtml)

instance (ToSample x x) => ToSample (Simple x) (Simple x) where
    toSample _ = fmap Tagged $ toSample (Proxy :: Proxy x)
instance (ToJSON x) => ToJSON (Simple x) where
    toJSON = toJSON . untag


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
    toHtml    = maybe (toHtml (""::T.Text))    (toHtml    . toSimple) . untag
    toHtmlRaw = maybe (toHtmlRaw (""::T.Text)) (toHtmlRaw . toSimple) . untag

-- HTML form of a single row
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

-- HTML form of a list of rows
instance    ( ToHtml (Simple ar)
            , Names (NRec (RecordDef a))
            , Rep rep (RecordDef a) ar
            , RecLens rep (RecordDef a) (Key a) ar kr
            , IsElem (PersRecAPI rep SimpleHtml b a ar kr dr)
                     (PersAPI rep SimpleHtml b a ar kr dr)
            , HasLink (PersRecAPI rep SimpleHtml b a ar kr dr)
            , Curring rep kr
            , Curried rep kr URI ~ MkLink (PersRecAPI rep SimpleHtml b a ar kr dr)
            )
    => ToHtml
        (Simple
            (Tagged '(rep,(b:: *),(a::DataDef *),ar,(kr:: *),(dr:: *)) [ar])
        )
  where
    toHtml (Tagged (Tagged xs)) = do
        -- termWith "script" [src_ "static/jq.js"] ""
        -- termWith "script" [src_ "static/api.js"] ""
        table_ $ do
            tr_ $ do
                th_ $ label_ "=>"
                foldMap (th_ . toHtml)
                    $ names (proxy# :: Proxy# (NRec (RecordDef a)))
            foldMap (\x -> tr_ $ do
                    td_ $ a_ [href_ $ ref x, target_ "blank"] $ button_ "=>"
                    -- td_ $ button_ [id_ "e", onclick_ "gettab1()"] "=>"
                    toHtml $ toSimple x
                ) xs
      where
        lpk = lensPk (Proxy :: Proxy '(rep,a))
        vpk rec = rec ^. lpk :: kr
        api = Proxy :: Proxy (PersAPI rep SimpleHtml (b:: *) a ar kr dr)
        recApi = Proxy :: Proxy (PersRecAPI rep SimpleHtml b a ar kr dr)
        ref = T.pack . show . lnk
        lnk rec = (uncurryN pRep $ safeLink api recApi) $ vpk rec :: URI
        pRep  = Proxy :: Proxy rep


    toHtmlRaw = toHtml

-- HTML form of a record
instance ToHtml (Simple (Tagged '(Plain, b, '[]) ()))
  where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

instance (ToHtml (Tagged Input x), ToHtml (Simple (Tagged '(Plain, False, rs) xs)),
        ToHtml (Proxy '(r, HasDef x)))
        => ToHtml (Simple (Tagged '(Plain, False, '(r,t) ': rs) (x,xs)))
  where
    toHtml (Tagged (Tagged (x,xs))) = do
        -- termWith "script" [src_ "static/jq.js"] ""
        -- termWith "script" [src_ "static/api.js"] ""
        tr_ $ do
            td_ $ toHtml (Proxy :: Proxy '(r,HasDef x))
            td_ $ toHtml $ toInput x
        toHtml $ toSimple (Tagged xs :: Tagged '(Plain, False, rs) xs)
    toHtmlRaw = toHtml

instance ToHtml (Simple (Tagged '(Plain, False, r ': rs) (x,xs)))
        => ToHtml (Simple (Tagged '(Plain, r ': rs) (x,xs)))
  where
    toHtml zs = do
        table_ $ toHtml
            (fmap retag zs :: Simple (Tagged '(Plain, False, r ': rs) (x,xs)))
        termWith "button" [class_ "btnSave"] "Save"
    toHtmlRaw = toHtml

instance ToHtml (Simple (Tagged '(Plain, r ': rs) (x,xs)))
        => ToHtml (Simple (Tagged '(Plain, r ': rs) (Maybe (x,xs))))
  where
    toHtml
        = maybe (p_ "No data found!")
                ( toHtml . toSimple
                . (Tagged :: (x,xs) -> Tagged '(Plain, r ': rs) (x,xs))
                )
        . untag . untag
    toHtmlRaw = toHtml

instance KnownSymbol s => ToHtml (Proxy '(s, True)) where
    toHtml    _ = toHtml    $ toSimple $ T.pack $ symbolVal' (proxy# :: Proxy# s)
    toHtmlRaw _ = toHtmlRaw $ toSimple $ T.pack $ symbolVal' (proxy# :: Proxy# s)

instance ToHtml (Proxy '(s, True)) => ToHtml (Proxy '(s, False)) where
    toHtml    _ = toHtml    (Proxy :: Proxy '(s,True)) >> span_ [class_ "rq_ast"] "*"
    toHtmlRaw _ = toHtmlRaw (Proxy :: Proxy '(s,True)) >> span_ [class_ "rq_ast"] "*"
