{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module Pers.Types
    where

import Data.Singletons.Prelude
import Data.Singletons.TypeRepStar()
import Data.Promotion.Prelude.List
import GHC.TypeLits -- (Symbol, KnownSymbol, SomeSymbol(..), KnownNat, symbolVal, natVal)
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality -- (type (==))
import Lens.Micro
-- import Control.Lens

import Pers.TH

-- using sMap and Map
-- :t sMap (singFun1 (Proxy::Proxy SndSym0) sSnd) (sing :: Sing '[ '("x",False), '("y",True) ])
-- :t Proxy :: Proxy (Map SndSym0 '[ '("x",False)])

-- | Пара Описание и Тип-значение
type (:::) (a :: k1) (b :: k2) = '(a,b)
infixl 9 :::

getSymbol :: (KnownSymbol n) => Proxy (n:::v) -> String
getSymbol (_ :: Proxy (n:::v)) = symbolVal' (proxy# :: Proxy# n)

getNat :: (KnownNat n) => Proxy (n:::v) -> Integer
getNat (_ :: Proxy (n:::v)) = natVal' (proxy# :: Proxy# n)

-- | Kind for type of representation
data Rep = Plain

-- | Type to define simple record representation as tuple (a,).(b,).(c,)....(z,) $ ()
-- type family Plain :: Rep

type family ToRep (rep::Rep) (a :: [*]) :: * where
    ToRep Plain '[] = ()
    ToRep Plain (x ': xs) = (x, ToRep Plain xs)

class Single (rep::Rep) where
    single :: Proxy# rep -> a -> VRec rep '["_":::a]
instance Single Plain where
    single _ = (,())


type NRec (a :: [(k,*)]) = Map FstSym0 a
type VRec (rep::Rep) (a :: [(k,*)]) = ToRep rep (Map SndSym0 a)
pNRec :: Proxy a -> Proxy (NRec a)
pNRec (_::Proxy a) = Proxy :: Proxy (NRec a)

type MinusNames (a :: [(k,*)]) (b :: [k])
    = {- If (NRec a == b) '[] -} (MinusBy EqFstSym0 a b)
type ProjNames  (a :: [(k,*)]) (b :: [k])
    = {- If (NRec a == b) a   -} (ProjBy EqFstSym0 a b)


class Names (x :: [Symbol]) where
    symbols :: Proxy# x -> [SomeSymbol]
    names   :: Proxy# x -> [String]
instance Names '[] where
    symbols _ = []
    names _ = []
instance (KnownSymbol s, Names ss) => Names (s ': ss) where
    symbols _ = SomeSymbol (Proxy :: Proxy s) : symbols (proxy# :: Proxy# ss)
    names _ = symbolVal' (proxy# :: Proxy# s) : names (proxy# :: Proxy# ss)


{-
-- | Construct Named Record value by adding values
class AddRec (rep::Rep) (a::[(k,*)]) (b::[(k,*)])
  where
    addRec :: Proxy# '(rep,a,b) -> VRec rep a -> VRec rep b -> VRec rep (a :++ b)

instance AddRec rep '[] b where
    addRec _ _ = id

instance (Elem a c ~ False, AddRec Plain b c) => AddRec Plain (a ': b) c where
    addRec _ (x,y) z = (x, addRec (proxy# :: Proxy# '(Plain,b,c)) y z)
-}

-- | Lens
class RecLens (rep::Rep) b a where
    recLens :: (Functor f) => Proxy# '(rep,b,a)
            -> (VRec rep a -> f (VRec rep a)) -> VRec rep b -> f (VRec rep b)

class RecLensB (rep::Rep) b a (isEq :: Bool) where
    recLensB :: (Functor f) => Proxy# '(rep,b,a) -> Proxy# (isEq::Bool)
            -> (VRec rep a -> f (VRec rep a)) -> VRec rep b -> f (VRec rep b)

instance (RecLensB rep (b ': bs) '[a] (a==b)) => RecLens rep (b ': bs) (a ': '[])
  where
    recLens p = recLensB p (proxy# :: Proxy# (a == b))

instance RecLensB Plain (a ': bs) '[a] True where
    recLensB _ _ f (x,y) = fmap ((,y).fst) $ f (x,())

instance (RecLens Plain bs '[a]) => RecLensB Plain (b ': bs) '[a] False where
    recLensB _ _ f (x,y) = fmap (x,)
                         $ recLens (proxy# :: Proxy# '(Plain,bs,'[a])) f y

instance ( RecLens Plain cs '[a], RecLens Plain cs (a1 ': as)
         ) => RecLens Plain cs (a ': a1 ': as)
  where
    recLens _ = lens get set
      where
        get = (\(a,_) b -> (a, b))
            <$> (^. recLens (proxy# :: Proxy# '(Plain,cs,'[a])))
            <*> (^. recLens (proxy# :: Proxy# '(Plain,cs,(a1 ': as))))
        set = (\x (v1,v2) -> x
                & recLens (proxy# :: Proxy# '(Plain,cs,'[a])) .~ (v1,())
                & recLens (proxy# :: Proxy# '(Plain,cs,(a1 ': as))) .~ v2
            )




-- -- test:
-- type A1 = '["x":::Int,"y":::Char,"z":::String]
-- x = (2::Int,).('x',).("sss",) $ ()
-- f = recLens (proxy# :: Proxy# '(Plain, A1, '["z":::String, "x":::Int]))
--     (\(c,(n,_)) -> [(c++" new",(n+1,()))])
