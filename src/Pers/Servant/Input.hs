{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pers.Servant.Input where

import Data.Tagged
import Lucid
import qualified Data.Text as T
import Servant(toText)
import Data.Int(Int64)
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal')

data Input

toInput :: Proxy (r::Symbol) -> a -> Tagged '(Input,r) a
toInput (_ :: Proxy r) = Tagged

getId :: KnownSymbol r => Tagged '(Input,r) a -> T.Text
getId (_ :: Tagged '(Input,r) a) = T.pack $ symbolVal' (proxy# :: Proxy# r)

instance KnownSymbol r => ToHtml (Tagged '(Input,r) Int64) where
    toHtml v@(Tagged x) = input_ [ id_ $ getId v
                                 , type_ "number"
                                 , required_ "true"
                                 , value_ $ toText x
                                 ]
    toHtmlRaw = toHtml

instance KnownSymbol r => ToHtml (Tagged '(Input,r) Double) where
    toHtml v@(Tagged x) = input_ [ id_ $ getId v
                                 , type_ "number"
                                 , required_ "true"
                                 , value_ $ toText x
                                 ]
    toHtmlRaw = toHtml

instance KnownSymbol r => ToHtml (Tagged '(Input,r) T.Text) where
    toHtml v@(Tagged x) = input_ [ id_ $ getId v
                                 , type_ "text"
                                 , required_ "true"
                                 , value_ $ toText x
                                 ]
    toHtmlRaw = toHtml

instance KnownSymbol r => ToHtml (Tagged '(Input,r) (Maybe Int64)) where
    toHtml v@(Tagged x) = input_ [ id_ $ getId v
                                 , type_ "number"
                                 , value_ $ maybe "" toText x
                                 ]
    toHtmlRaw = toHtml

instance KnownSymbol r => ToHtml (Tagged '(Input,r) (Maybe Double)) where
    toHtml v@(Tagged x) = input_ [ id_ $ getId v
                                 , type_ "number"
                                 , value_ $ maybe "" toText x
                                 ]
    toHtmlRaw = toHtml

instance KnownSymbol r => ToHtml (Tagged '(Input,r) (Maybe T.Text)) where
    toHtml v@(Tagged x) = input_ [ id_ $ getId v
                                 , type_ "text"
                                 , value_ $ maybe "" toText x
                                 ]
    toHtmlRaw = toHtml
