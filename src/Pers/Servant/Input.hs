{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Pers.Servant.Input where

import Data.Tagged
import Lucid
import qualified Data.Text as T
import Servant(toText)
import Data.Int(Int64)


data Input

toInput :: a -> Tagged Input a
toInput = Tagged

instance ToHtml (Tagged Input Int64) where
    toHtml (Tagged x) = input_ [type_ "number", required_ "true", value_ $ toText x]
    toHtmlRaw = toHtml

instance ToHtml (Tagged Input Double) where
    toHtml (Tagged x) = input_ [type_ "number", required_ "true", value_ $ toText x]
    toHtmlRaw = toHtml

instance ToHtml (Tagged Input T.Text) where
    toHtml (Tagged x) = input_ [type_ "text", value_ $ toText x]
    toHtmlRaw = toHtml

instance ToHtml (Tagged Input (Maybe Int64)) where
    toHtml (Tagged x) = input_ [type_ "number", value_ $ maybe "" toText x]
    toHtmlRaw = toHtml

instance ToHtml (Tagged Input (Maybe Double)) where
    toHtml (Tagged x) = input_ [type_ "number", value_ $ maybe "" toText x]
    toHtmlRaw = toHtml

instance ToHtml (Tagged Input (Maybe T.Text)) where
    toHtml (Tagged x) = input_ [type_ "text", value_ $ maybe "" toText x]
    toHtmlRaw = toHtml
