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
    , AddRec(..)
    , FieldLens(..)
    , Has(..)
    , Lifted
    , ToRec(..)
    ) where

import Data.Either(lefts)
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol, KnownSymbol, SomeSymbol(..))
import Data.Type.Bool(type (&&), type (||))
import Lens.Micro -- (_1, _2)
import Data.Default(Default(..))

import Data.Map(Map)
import qualified Data.Map as M


import NamedValue((:>)(..))

infixl 6 +>

------------ Construction ----------------
class AddRec a b where
    (+>) :: a -> b -> a +> b

instance AddRec (n1:>v1) (n2:>v2) where
    a +> b = (a,b)

instance (AddRec' (EqCnt a b) a b (n :> v)) => AddRec (a,b) (n:>v) where
    x +> y = add (Proxy :: Proxy (EqCnt a b)) x y

class AddRec' (x::Bool) a b nv where
    add :: Proxy x -> (a,b) -> nv -> Add x a b nv

instance (AddRec a nv) => AddRec' True a b nv where
    add _ (x,y) nv = (x +> nv, y)

instance (AddRec b nv) => AddRec' False a b nv where
    add _ (x,y) nv = (x, y +> nv)

type family (+>) a b where
    (+>) (n1:>v1) (n2:>v2)  = (n1:>v1, n2:>v2)
    (+>) (a, b) (n:>v)      = Add (EqCnt a b) a b (n:>v)

type family EqCnt a b :: Bool where
    EqCnt (n1:>v1) (n2:>v2) = True
    EqCnt (a,b) (n:>v) = False
    EqCnt (a,b) (c,d) = (EqCnt a c) && (EqCnt b d)

type family Add (x::Bool) a b nv where
    Add True a b nv = (a +> nv, b)
    Add False a b nv = (a, b +> nv)

---------- Lens -----------------------
{-
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
-}
type family Has a b :: Bool where
    Has ((n::Symbol):>v) (n:>v) = True
    Has (a,b) (n:>v) = (Has a (n:>v)) || (Has b (n:>v))
    Has a (b,c) = (Has a b) && (Has a c)
    Has a b = False

class (Has a b ~ True) => FieldLens a b where
    fldLens :: (Functor f) => (b -> f b) -> a -> f a

class FieldB a b c (isLeft::Bool) where
    fldB :: (Functor f)
        => Proxy isLeft -> (c -> f c) -> (a,b) -> f (a,b)

instance (FieldLens a с) => FieldB a b с True where
    fldB _ = _1 . fldLens

instance (FieldLens b с) => FieldB a b с False where
    fldB _ = _2 . fldLens

instance FieldLens (n:>v) (n:>v) where
    fldLens = id

instance ((Has a (n:>v) || Has b (n:>v)) ~ True, FieldB a b (n:>v) (Has a (n:>v)))
    => FieldLens (a,b) (n:>v)
  where
    fldLens = fldB (Proxy :: Proxy (Has a (n:>v)))

instance ((Has a b && Has a c) ~ True, FieldLens a b, FieldLens a c)
    => FieldLens a (b,c)
  where
    fldLens = lens  ((,) <$> (^. fldLens) <*> (^. fldLens))
                    (\x (v1,v2) -> x & fldLens .~ v1 & fldLens .~ v2)
        -- fldB (Proxy :: Proxy (Has a (n:>v)))

--------- Initialization, Conversion ----------------
type family Lifted f a where
    Lifted f (n:>v) = n :> (f v)
    Lifted f (a,b) = (Lifted f a, Lifted f b)

class ToRec a where
    toRec :: Lifted Maybe a -> Either [SomeSymbol] a

instance (KnownSymbol n, ToRecDef (HasDef v) v) => ToRec (n:>v) where
    toRec (V mv)
        = maybe ( maybe
                    (Left [SomeSymbol (Proxy::Proxy n)])
                    (Right . V)
                    $ toRecDef (Proxy :: Proxy (HasDef v))
                )
                (Right . V)
                mv

instance (ToRec a, ToRec b) => ToRec (a,b) where
    toRec (a,b) = case (toRec a, toRec b) of
        (Right x, Right y) -> Right (x,y)
        (x, y) -> Left $ concat $ lefts [x] ++ lefts [y]

type family HasDef a :: Bool where
    HasDef (Maybe a) = True
    HasDef String = False
    HasDef [a] = True
    HasDef a = False

class ToRecDef (b::Bool) a where
    toRecDef :: Proxy b -> Maybe a

instance (Default a) => ToRecDef True a where
    toRecDef _ = Just def

instance ToRecDef False a where
    toRecDef _ = Nothing
