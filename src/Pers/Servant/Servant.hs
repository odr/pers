{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Pers.Servant.Servant where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Servant
import Servant.HTML.Lucid(HTML)

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML

-- | Type to default class instances
data ServantDefault

class PersServant opt back (x::k) where
    type PersAPI   opt back x
    type PersMonad opt back x :: * -> *
    persServer :: Proxy# opt -> Proxy# back -> Proxy x
        -> ServerT (PersAPI opt back x) (PersMonad opt back x)

persServer' :: PersServant ServantDefault back x
    => Proxy# back -> Proxy x
    -> ServerT (PersAPI ServantDefault back x) (PersMonad ServantDefault back x)
persServer' = persServer (proxy# :: Proxy# ServantDefault)

instance (DBOption back, DML Plain back (TableDef n rec pk))
    => PersServant ServantDefault back (TableDef n rec pk)
  where
    type PersAPI ServantDefault back (TableDef n rec pk)
        = n :> "list" :> Get '[JSON,HTML] [VRec Plain rec]
    type PersMonad ServantDefault back (TableDef n rec pk)
        = SessionMonad back IO
    persServer _ _ _ = undefined

