module Pers.Servant.Lucid where
import Lucid
import Data.Int(Int64)

instance ToHtml Int64 where
    toHtml = toHtml . show
    toHtmlRaw = toHtml

instance ToHtml Double where
    toHtml = toHtml . show
    toHtmlRaw = toHtml

instance (ToHtml a) => ToHtml (Maybe a) where
    toHtml = maybe (toHtml "") toHtml
    toHtmlRaw = maybe (toHtmlRaw "") toHtmlRaw

