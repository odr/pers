{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Int
import Data.List
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Servant.HTML.Lucid(HTML)
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

main :: IO ()
main = run 8081 app

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve (Proxy :: Proxy (UserAPI :<|> PersonAPI)) (server :<|> server4)

type API = UserAPI :<|> PersonAPI

type UserAPI = "users" :> Get '[JSON] [User]
            :<|> "albert" :> Get '[JSON] User
            :<|> "isaac" :> Get '[JSON] User

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

--------------
-------------

data User = User
  { name :: String
  , age :: Int
  , email :: String
  -- , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" -- (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" -- (fromGregorian 1905 12 1)

users :: [User]
users = [isaac, albert]

server :: Server UserAPI
server = return users
        :<|> return albert
        :<|> return isaac

server4 = return persons

userAPI :: Proxy UserAPI
userAPI = Proxy

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- for the JSON instance

instance ToJSON Person

-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml

persons :: [Person]
persons =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

