{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
module Pers.Types
    where

import Data.Singletons.Prelude(Map, FstSym0, SndSym0)
-- import Data.Singletons.TypeRepStar()
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', SomeSymbol(..))
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality(type (==))
import Lens.Micro(Lens', (^.), (.~), (&), lens)
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

-- | Kind for type of representation. Constructors are types.
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

type family NRec (a :: [(k1,k2)]) :: [k1] where
    NRec '[] = '[]
    NRec ('(a,b) ': xs) = a ': NRec xs
-- type NRec (a :: [(k,*)]) = Map FstSym0 a
type VRec (rep::Rep) (a :: [(k,*)]) = ToRep rep (Map SndSym0 a)
{-
type family VRec' (rep::Rep) (a :: [(k,*)]) :: * where
    VRec' Plain '[] = ()
    VRec' Plain '[ '(a,b)] = (b,())
    VRec' Plain ('(a,b) ': xs) = (b, VRec' Plain xs)
-}

pNRec :: Proxy a -> Proxy (NRec a)
pNRec (_::Proxy a) = Proxy :: Proxy (NRec a)


-- type MinusNames (a :: [(k,*)]) (b :: [k])
--     = (MinusBy EqFstSym0 a b)
type family MinusNames (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    MinusNames xs '[] = xs
    MinusNames ( '(a,b) ': xs ) '[a] = xs
    MinusNames ( '(a,b) ': xs ) '[c] = '(a,b) ': MinusNames xs '[c]
    MinusNames xs (y ': ys) = MinusNames (MinusNames xs '[y]) ys

type family NamesMinus (b :: [k]) (a :: [(k,*)]) :: [k] where
    NamesMinus xs '[] = xs
    NamesMinus ( a ': xs ) '[ '(a,b)] = xs
    NamesMinus ( a ': xs ) '[c] =  a ': NamesMinus xs '[c]
    NamesMinus xs (y ': ys) = NamesMinus (NamesMinus xs '[y]) ys

-- type ProjNames  (a :: [(k,*)]) (b :: [k])
--     = (ProjBy EqFstSym0 a b)
type family ProjNames  (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    ProjNames xs '[] = '[]
    ProjNames xs '[c] = '[ProjName xs c]
    ProjNames xs (y ': ys) = ProjName xs y ': ProjNames xs ys

type family ProjName  (a :: [(k,*)]) (b :: k) where
    ProjName ( '(a,b) ': xs ) a = '(a,b)
    ProjName ( '(a,b) ': xs ) c = ProjName xs c

class Names (x :: [Symbol]) where
    symbols :: Proxy# x -> [SomeSymbol]
    names   :: Proxy# x -> [String]
instance Names '[] where
    symbols _ = []
    names _ = []
instance (KnownSymbol s, Names ss) => Names (s ': ss) where
    symbols _ = SomeSymbol (Proxy :: Proxy s) : symbols (proxy# :: Proxy# ss)
    names _ = symbolVal' (proxy# :: Proxy# s) : names (proxy# :: Proxy# ss)

-- | Lens
class RecLens (rep::Rep) (b::[(k,*)]) (a::[(k,*)]) where
    recLens :: Proxy# '(rep,b,a) -> Lens' (VRec rep b) (VRec rep a)

class RecLensB (rep::Rep) (b::[(k,*)]) (a::[(k,*)]) (isEq :: Bool) where
    recLensB :: Proxy# '(rep,b,a) -> Proxy# (isEq::Bool)
                -> Lens' (VRec rep b) (VRec rep a)

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

recLens' :: RecLens rep b (ProjNames b a)
        => Proxy '(rep,b,a) -> Lens' (VRec rep b) (VRec rep (ProjNames b a))
recLens' (_:: Proxy '(rep,b,a))
    = recLens (proxy# :: Proxy# '(rep, b, ProjNames b a))

{-
-- | Construct Named Record value by adding values
class AddRec (rep::Rep) (a::[(k,*)]) (b::[(k,*)])
class AddRec (rep::Rep) (a::[(k,*)]) (b::[(k,*)])
  where
    addRec :: Proxy# '(rep,a,b) -> VRec rep a -> VRec rep b -> VRec rep (a :++ b)

instance AddRec rep '[] b where
    addRec _ _ = id

instance (Elem a c ~ False, AddRec Plain b c) => AddRec Plain (a ': b) c where
    addRec _ (x,y) z = (x, addRec (proxy# :: Proxy# '(Plain,b,c)) y z)
-}


-- -- test:
-- type A1 = '["x":::Int,"y":::Char,"z":::String]
-- x = (2::Int,).('x',).("sss",) $ ()
-- f = recLens (proxy# :: Proxy# '(Plain, A1, '["z":::String, "x":::Int]))
--     (\(c,(n,_)) -> [(c++" new",(n+1,()))])
