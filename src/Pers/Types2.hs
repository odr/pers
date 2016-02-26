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
{-# LANGUAGE FunctionalDependencies #-}
module Pers.Types2
    where

-- import Data.Singletons.Prelude(Map, FstSym0, SndSym0)
-- import Data.Singletons.TypeRepStar()
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', SomeSymbol(..))
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality(type (==))
import Lens.Micro(Lens', (^.), (.~), (&), lens)
import GHC.Exts(Constraint)
-- import Control.Lens

-- import Pers.TH

-- using sMap and Map
-- :t sMap (singFun1 (Proxy::Proxy SndSym0) sSnd) (sing :: Sing '[ '("x",False), '("y",True) ])
-- :t Proxy :: Proxy (Map SndSym0 '[ '("x",False)])

-- | Пара Описание и Тип-значение
type (:::) (a :: k1) (b :: k2) = '(a,b)
infixl 9 :::

getSymbol :: (KnownSymbol n) => Proxy (n:::v) -> String
getSymbol (_ :: Proxy (n:::v)) = symbolVal' (proxy# :: Proxy# n)

-- | Kind for type of representation. Constructors are types.
data R = Plain

-- | Type to define simple record representation as tuple (a,).(b,).(c,)....(z,) $ ()

class Single (rep::R) where
    type Singl rep a
    single :: Proxy# rep -> a -> Singl rep a
instance Single Plain where
    type Singl Plain a = (a,())
    single _ = (,())

class Rep (rep::R) (a :: [(k,*)]) (b :: *) | rep a -> b
instance Rep Plain '[] ()
instance (Rep Plain xs c) => Rep Plain ('(a,b) ': xs) (b, c)

type family VRec (rep::R) (a :: [(k,*)]) :: * where
    VRec Plain '[] = ()
    VRec Plain '[ '(a,b)] = (b,())
    VRec Plain ('(a,b) ': xs) = (b, VRec Plain xs)

class (Rep rep b br, Rep rep a ar)
    => RecLens rep b a br ar    | rep b -> br
                                , rep a -> ar
  where
    recLens :: Proxy# '(rep,b,a) -> Lens' br ar

instance    ( RecLensB (a==b) rep (b ': bs) '[a] br ar
            )
            => RecLens rep (b ': bs) (a ': '[]) br ar
  where
    recLens p = recLensB p (proxy# :: Proxy# (a == b))

instance    ( RecLens Plain bs '[a1] xs (y1,())
            , RecLens Plain bs (a2 ': as) xs (y2,ys)
            , Rep Plain (a1 ': a2 ': as) (y1,(y2,ys))
            )
            => RecLens Plain bs (a1 ': a2 ': as) xs (y1,(y2,ys))
  where
    recLens _ = lens get set
      where
        get = (\(a,_) b -> (a, b))
            <$> (^. recLens (proxy# :: Proxy# '(Plain,bs,'[a1])))
            <*> (^. recLens (proxy# :: Proxy# '(Plain,bs,(a2 ': as))))
        set = (\x (v1,v2) -> x
                & recLens (proxy# :: Proxy# '(Plain,bs,'[a1])) .~ (v1,())
                & recLens (proxy# :: Proxy# '(Plain,bs,(a2 ': as))) .~ v2
            )
class (Rep rep b br, Rep rep a ar)
    => RecLensB (eq::Bool) rep b a br ar
                | rep b -> br, rep a -> ar
  where
    recLensB :: Proxy# '(rep,b,a) -> Proxy# eq -> Lens' br ar

instance (Rep Plain (a ': as) (x,xs), Rep Plain '[a] (x,()))
    => RecLensB True Plain (a ': as) '[a] (x,xs) (x,())
  where
    recLensB _ _ f (x,y) = fmap ((,y).fst) $ f (x,())

instance (Rep Plain (b ': bs) (x,xs), RecLens Plain bs '[a] xs (y,()))
    => RecLensB False Plain (b ': bs) '[a] (x,xs) (y,())
  where
    recLensB _ _ f (x,y)
        = fmap (x,)
        $ recLens (proxy# :: Proxy# '(Plain,bs,'[a])) f y

type family NRec (a :: [(k1,k2)]) :: [k1] where
    NRec '[] = '[]
    NRec ('(a,b) ': xs) = a ': NRec xs

pNRec :: Proxy a -> Proxy (NRec a)
pNRec (_::Proxy a) = Proxy :: Proxy (NRec a)

type family MinusNames (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    MinusNames xs '[] = xs
    MinusNames ( '(a,b) ': xs ) '[a] = xs
    MinusNames ( '(a,b) ': xs ) '[c] = '(a,b) ': MinusNames xs '[c]
    MinusNames xs (y ': ys) = MinusNames (MinusNames xs '[y]) ys

type family ContainNames (a :: [(k,k2)]) (b :: [k]) :: Constraint where
    ContainNames as '[] = ()
    ContainNames ('(a,v) ': as) '[a] = ()
    ContainNames ('(a,v) ': as) '[b] = ContainNames as '[b]
    ContainNames as (b1 ': b2 ': bs)
        = (ContainNames as '[b1],  ContainNames as (b2 ': bs))

type family Contains (a::[k]) (b::[k]) :: Constraint where
    Contains as '[] = ()
    Contains (a ': as) '[a] = ()
    Contains (a ': as) '[b] = Contains as '[b]
    Contains as (b1 ': b2 ': bs)
        = (Contains as '[b1],  Contains as (b2 ': bs))

{-
type family NamesMinus (b :: [k]) (a :: [(k,*)]) :: [k] where
    NamesMinus xs '[] = xs
    NamesMinus ( a ': xs ) '[ '(a,b)] = xs
    NamesMinus ( a ': xs ) '[c] =  a ': NamesMinus xs '[c]
    NamesMinus xs (y ': ys) = NamesMinus (NamesMinus xs '[y]) ys

class Contains (a::[k]) (b::[k])
instance Contains as '[]
instance (ContainsB (a==b) (a ': as) '[b]) => Contains (a ': as) '[b]
instance (Contains as '[b1], Contains as (b2 ': bs))
            => Contains as (b1 ': b2 ': bs)

class ContainsB (eq :: Bool) (a :: [k]) (b :: [k])
instance (Contains as '[b]) => ContainsB False (a ': as) '[b]
instance ContainsB True (a ': as) '[a]

class ContainNames (a :: [(k,k2)]) (b :: [k])
instance ContainNames as '[]
instance (ContainNamesB (a==b) ('(a,v) ': as) '[b])
            => ContainNames ('(a,v) ': as) '[b]
instance (ContainNames as '[b1], ContainNames as (b2 ': bs))
            => ContainNames as (b1 ': b2 ': bs)

class ContainNamesB (eq :: Bool) (a :: [(k,k2)]) (b :: [k])
instance (ContainNames as '[b]) => ContainNamesB False ('(a,v) ': as) '[b]
instance ContainNamesB True ('(a,v) ': as) '[a]
-}

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

recLens'    ::  ( Rep rep b br
            , Rep rep (ProjNames b a) ar
            , RecLens rep b (ProjNames b a) br ar
            )
    => Proxy '(rep,b,a) -> Lens' br ar
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
