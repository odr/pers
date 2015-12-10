{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module PersistRec
    ( PersistRec(..)
    , mapToRec
    ) where

import Database.Persist(PersistValue(..), PersistField(..))
import Data.Either(either, lefts)
import Data.Proxy(Proxy(..))
import Data.Map(Map)
import qualified Data.Map as M
import GHC.TypeLits(SomeSymbol(..), KnownSymbol, symbolVal)

import NamedRecord((:>)(..), Lifted, ToRec(..))

-- | Conversion from and to 'PersistValue'
class PersistRec a where
    fromMap :: Proxy a -> Map SomeSymbol PersistValue -> Lifted Maybe a
    toMap :: a -> Map SomeSymbol PersistValue

instance (KnownSymbol n, PersistField v) => PersistRec (n:>v) where
    fromMap _ m = maybe (V Nothing) (V . Just)
                $ M.lookup (SomeSymbol (Proxy::Proxy n)) m
                >>= either (const Nothing) Just . fromPersistValue
    toMap (V x) = M.fromList [(SomeSymbol (Proxy::Proxy n), toPersistValue x)]

instance (PersistRec a, PersistRec b) => PersistRec (a,b) where
    fromMap _ m = (fromMap (Proxy :: Proxy a) m, fromMap (Proxy :: Proxy b) m)
    toMap = mappend <$> toMap . fst <*> toMap . snd

-- | Get Named Record (or list of unitialized fields) from @Map@
mapToRec :: (ToRec a, PersistRec a)
        => Proxy a -> Map SomeSymbol PersistValue -> Either [SomeSymbol] a
mapToRec p = toRec . fromMap p
