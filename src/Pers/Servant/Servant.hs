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
import Servant.HTML.Lucid -- (HTML)
import Control.Monad.Trans.Either

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML
-- import Pers.Servant.Lucid()

type PersMonad back = SessionMonad back (EitherT ServantErr IO)
newtype FieldHtml r a = FieldHtml { unFieldHtml :: a }

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


