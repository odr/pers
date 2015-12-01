{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module FromMap
    ( FromMap(..)
    , mapToRec
    ) where

import Database.Persist.Class(PersistField(..))
import Database.Persist.Types(PersistValue(..))
import Data.Either(either, lefts)
import Data.Proxy(Proxy(..))
import Data.Map(Map)
import qualified Data.Map as M
import GHC.TypeLits(SomeSymbol(..), KnownSymbol)

import NamedRecord((:>)(..), Lifted, ToRec(..))
--import Data.Text(Text)

class FromMap a where
    fromMap :: Proxy a -> Map SomeSymbol PersistValue -> Lifted Maybe a

instance (KnownSymbol n, PersistField v) => FromMap (n:>v) where
    fromMap _ m = maybe (V Nothing) (V . Just)
                $ M.lookup (SomeSymbol (Proxy::Proxy n)) m
                >>= either (const Nothing) Just . fromPersistValue

instance (FromMap a, FromMap b) => FromMap (a,b) where
    fromMap _ m
        = (fromMap (Proxy :: Proxy a) m, fromMap (Proxy :: Proxy b) m)

mapToRec :: (ToRec a, FromMap a)
        => Proxy a -> Map SomeSymbol PersistValue -> Either [SomeSymbol] a
mapToRec p = toRec . fromMap p

