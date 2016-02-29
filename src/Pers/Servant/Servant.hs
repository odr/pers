{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Pers.Servant.Lucid()

-- | Type to default class instances
data ServantDefault

type PersMonad back = SessionMonad back (EitherT ServantErr IO)

class PersServant opt back (x::k) where -- ar kr | opt x -> ar, opt x -> kr where
    type PersAPI   opt back x
    persServer :: Proxy# opt -> Proxy# back -> Proxy x
        -> ServerT (PersAPI opt back x) (PersMonad back)

persServer' :: PersServant ServantDefault back x -- ar kr
    => Proxy# back -> Proxy x
    -> ServerT (PersAPI ServantDefault back x) (PersMonad back)
persServer' = persServer (proxy# :: Proxy# ServantDefault)

instance    ( DBOption back
            , DML Plain back (TableDef n rec pk) (VRec Plain rec) (VRec Plain (ProjNames rec pk)) -- ar kr
            , ContainNames rec (NRec rec)
            , Names (NRec rec)
            , RowRepDDL 'Plain back (ProjNames rec (NRec rec)) (VRec Plain rec)
            )
    => PersServant ServantDefault back (TableDef n rec pk) -- ar kr
  where
    type PersAPI ServantDefault back (TableDef n rec pk)
        = n :> "list" :> Get '[JSON,HTML] (Proxy '(Plain, rec), [VRec Plain rec])

    persServer _ _ (_::Proxy (TableDef n rec pk)) = do
        fmap (Proxy :: Proxy '(Plain, rec),)
            $ sel (Proxy :: Proxy '(Plain, TableDef n rec pk)) mempty

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


