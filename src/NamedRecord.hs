{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module NamedRecord
    ( type (+>)
    , (:>)(..)
    , FieldLens(..)
    , Has(..)
--    , LiftedRec(..)
--    , maybeToRec
    , ToRec(..)
    , Lifted
    ) where

import Data.Either(lefts)
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal)
import Data.Type.Bool(type (&&), type (||))
import Lens.Simple(_1, _2)

import Control.Arrow
import Data.Map(Map)
import qualified Data.Map as M
import Data.Text(Text)
import qualified Data.Text as T

import NamedValue((:>)(..))
import FieldValue(FieldValue(..))

infixl 6 +>

type family (+>) a b where
    (+>) (n1:>v1) (n2:>v2)
        = (n1:>v1, n2:>v2)
    (+>)  (a, b) (n:>v)
        = Add (EqCnt a b) (a, b) (n:>v)

type family EqCnt a b :: Bool where
    EqCnt (n1:>v1) (n2:>v2) = True
    EqCnt (a,b) (n:>v) = False
    EqCnt (a,b) (c,d) = (EqCnt a c) && (EqCnt b d)

type family Add (x::Bool) a b where
    Add True (a,b) (n:>v) = (a +> n:>v, b)
    Add False (a,b) (n:>v) = (a, b +> n:>v)

type family Has a (n :: Symbol) v :: Bool where
    Has ((n::Symbol):>v) n v = True
    Has (a,b) n v = (Has a n v) || (Has b n v)
    Has a n v = False

class (Has a n v ~ True) => FieldLens a (n::Symbol) v where
    fldLens :: (Functor f) => Proxy (n:>v) -> (v -> f v) -> a -> f a

class FieldB a b n v (isLeft::Bool) where
    fldB :: (Functor f)
        => Proxy isLeft -> Proxy (n:>v) -> (v -> f v) -> (a,b) -> f (a,b)

instance (FieldLens a n v) => FieldB a b n v True where
    fldB _ p = _1 . fldLens p

instance (FieldLens b n v) => FieldB a b n v False where
    fldB _ p = _2 . fldLens p

instance FieldLens (n:>v) n v where
    fldLens _ f (V v) = fmap V $ f v

instance ((Has a n v || Has b n v) ~ True, FieldB a b n v (Has a n v))
    => FieldLens (a,b) n v
  where
    fldLens = fldB (Proxy :: Proxy (Has a n v))

type family Lifted f a where
    Lifted f (n:>v) = n :> (f v)
    Lifted f (a,b) = (Lifted f a, Lifted f b)

class ToRec a where
    toRec :: Lifted Maybe a -> Either [Text] a
    fromMap :: (FieldValue fv) => Proxy a -> Map Text fv -> Lifted Maybe a -> Lifted Maybe a

instance (KnownSymbol n) => ToRec (n:>v) where
    toRec (V mv) = maybe (Left [T.pack $ symbolVal (Proxy::Proxy n)])
                        (Right . V) mv
    fromMap _ m _= maybe (V Nothing) (V . Just)
                        (M.lookup (T.pack $ symbolVal (Proxy::Proxy n)) m
                        >>= fromFieldValue
                        )

instance (ToRec a, ToRec b) => ToRec (a,b) where
    toRec (a,b) = case (toRec a, toRec b) of
        (Right x, Right y) -> Right (x,y)
        (x, y) -> Left $ concat $ lefts [x] ++ lefts [y]
    fromMap _ m (x, y)
        = (fromMap (Proxy :: Proxy a) m x, fromMap (Proxy :: Proxy b) m y)
{-
class LiftedRec (f :: * -> *) a where
    type Lifted f a
    toRec :: (forall v. f v -> Maybe v) -> Lifted f a -> Either [Text] a

instance (KnownSymbol n) => LiftedRec f (n:>v) where
    type Lifted f (n:>v) = n :> (f v)
    toRec g (V fv) = maybe (Left [T.pack $ symbolVal (Proxy::Proxy n)]) (Right . V)
                $ g fv

instance (LiftedRec f a, LiftedRec f b) => LiftedRec f (a,b) where
    type Lifted f (a,b) = (Lifted f a, Lifted f b)
    toRec g (a,b) = case (toRec g a, toRec g b) of
        (Right x, Right y) -> Right (x,y)
        (x, y) -> Left $ concat $ lefts [x] ++ lefts [y]

maybeToRec :: (LiftedRec Maybe a) => Lifted Maybe a -> Either [Text] a
maybeToRec = toRec id

mapToRec :: (LiftedRec Maybe a) => Map Text fv -> Either [Text] a
mapToRec = undefined
-}
