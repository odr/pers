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
module NamedRecord2
    where

import Data.Singletons.Prelude
import Data.Singletons.TypeRepStar()
import Data.Promotion.Prelude.List
import GHC.TypeLits -- (Symbol, KnownSymbol, SomeSymbol(..), KnownNat, symbolVal, natVal)
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality -- (type (==))
--import Lens.Micro
import Control.Lens

import NRTH

--   where
--     type (:==) (x:: *) (y:: *) = x == y

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

type family ToRep (a :: [*]) :: * where
    ToRep '[] = ()
    ToRep (x ': xs) = (x, ToRep xs)

{-
type family FromRep (a :: *) :: [*] where
    FromRep () = '[]
    FromRep (x,y) = x ': FromRep y
-}

type NRec (a :: [(k,*)]) = Map FstSym0 a
type VRec (a :: [(k,*)]) = Map SndSym0 a
-- type NVRec (a :: [k]) (b :: [*]) = Zip a b
type VRecRep (a :: [(k,*)]) = ToRep (VRec a)
-- type NVRecRep (a :: [k]) (b :: *) = NVRec a (FromRep b)
pNRec :: Proxy a -> Proxy (NRec a)
pNRec (_::Proxy a) = Proxy :: Proxy (NRec a)

type MinusNames (a :: [(k,*)]) (b :: [k]) = MinusBy EqFstSym0 a b
type ProjNames  (a :: [(k,*)]) (b :: [k]) = ProjBy EqFstSym0 a b

class Names (x :: [Symbol]) where
    symbols :: Proxy# x -> [SomeSymbol]
    names   :: Proxy# x -> [String]
instance Names '[] where
    symbols _ = []
    names _ = []
instance (KnownSymbol s, Names ss) => Names (s ': ss) where
    symbols _ = SomeSymbol (Proxy :: Proxy s) : symbols (proxy# :: Proxy# ss)
    names _ = symbolVal' (proxy# :: Proxy# s) : names (proxy# :: Proxy# ss)

-- | Construct Named Record value by adding values
class AddRec (a::[(k,*)]) (b::[(k,*)])
  where
    addRec :: (Proxy# a) -> (Proxy# b) -> VRecRep a -> VRecRep b -> VRecRep (a :++ b)

instance AddRec '[] b where
    addRec _ _ _ = id
instance (Elem a c ~ False, AddRec b c) => AddRec (a ': b) c where
    addRec _ _ (x,y) z = (x, addRec (proxy# :: Proxy# b) (proxy# :: Proxy# c) y z)

-- | Lens
class RecLens b a where
    recLens :: (Functor f) => Proxy# b -> Proxy# a
            -> (VRecRep a -> f (VRecRep a)) -> VRecRep b -> f (VRecRep b)
class RecLensB b a (isEq :: Bool) where
    recLensB :: (Functor f) => Proxy# b -> Proxy# a -> Proxy# isEq
            -> (VRecRep a -> f (VRecRep a)) -> VRecRep b -> f (VRecRep b)
instance (RecLensB (b ': bs) '[a] (a==b)) => RecLens (b ': bs) (a ': '[])
  where
    recLens pb pa = recLensB pb pa (proxy# :: Proxy# (a == b))
instance RecLensB (a ': bs) '[a] True where
    recLensB _ _ _ f (x,y) = fmap ((,y).fst) $ f (x,())
instance (RecLens bs '[a]) => RecLensB (b ': bs) '[a] False where
    recLensB _ pa _ f (x,y) = fmap (x,) $ recLens (proxy# :: Proxy# bs) pa f y

instance ( RecLens cs '[a], RecLens cs (a1 ': as)
         ) => RecLens cs (a ': a1 ': as)
  where
    recLens pc _ = lens get set
      where
        get = (\(a,_) b -> (a, b))
            <$> (^. recLens pc (proxy# :: Proxy# '[a]))
            <*> (^. recLens pc (proxy# :: Proxy# (a1 ': as)))
            :: VRecRep cs -> VRecRep (a ': a1 ': as)
        set = (\x (v1,v2) -> x
                        & recLens pc (proxy# :: Proxy# '[a]) .~ (v1,())
                        & recLens pc (proxy# :: Proxy# (a1 ': as)) .~ v2
            )
            :: VRecRep cs -> VRecRep (a ': a1 ': as) -> VRecRep cs




-- -- test:
-- type A1 = '["x":::Int,"y":::Char,"z":::String]
-- x = (1,).('x',).("sss",) $ () :: VRecRep A1
-- f = recLens (proxy# :: Proxy# '["z":::String, "x":::Int]) (proxy# :: Proxy# A1)
--     (\(c,(n,_)) -> [(c++" new",(n+1,()))])
