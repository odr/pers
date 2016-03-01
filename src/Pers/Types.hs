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
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE OverlappingInstances #-}
module Pers.Types
    where

import GHC.TypeLits(Symbol, KnownSymbol, symbolVal', SomeSymbol(..))
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality(type (==))
import Lens.Micro(Lens', (^.), (.~), (&), lens)
import GHC.Exts(Constraint)
import qualified Data.Text as T
import Data.Aeson(ToJSON(..),FromJSON(..),Value(..)
                ,fromJSON, Result(..), object, (.:))
import Data.Aeson.Types(Parser)
import Control.Monad(mzero)
import qualified Data.HashMap.Strict as HM
import Lucid

-- | Пара Описание и Тип-значение
type (:::) (a :: k1) (b :: k2) = '(a,b)
infixl 9 :::

-- | Kind for type of representation. Constructors are types.
data R
    = Plain -- ^ Type to define simple record representation as tuple (a,).(b,).(c,)....(z,) $ ()

-- | Representation of singleton
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

type family MinusNames (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    MinusNames xs '[] = xs
    MinusNames ( '(a,b) ': xs ) '[a] = xs
    MinusNames ( '(a,b) ': xs ) '[c] = '(a,b) ': MinusNames xs '[c]
    MinusNames xs (y ': ys) = MinusNames (MinusNames xs '[y]) ys

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

-----------------
-- data Typer a v = Typer { pTyper :: Proxy a, unTyper :: v } deriving Functor
--
-- instance Applicative (Typer a) where
--     pure v = Typer (Proxy :: Proxy a) v
--     (Typer p1 f) <*> (Typer p2 g) = Typer p2 $ f g
--     (*>) = flip const
--     (<*) = const
--
-- instance Monad (Typer a) where
--     (Typer _ a) >>= f = f a
--
-- instance (ToJSON v) => ToJSON (Typer a v) where
--     toJSON = toJSON . unTyper
--
-- instance (FromJSON v) => FromJSON (Typer a v) where
--     parseJSON = fmap pure . parseJSON
---------------

class (Rep rep a ar)
    => ToPairs (rep::R) (a::[(Symbol,*)]) ar | rep a -> ar
  where
    toPairs     :: Proxy# rep -> Proxy a -> ar
                -> [(T.Text, Value)] -> [(T.Text, Value)]

instance ToPairs Plain ('[]) () where
    toPairs   _ _ _ = id

instance    ( KnownSymbol n
            , ToPairs Plain nvs vr
            , ToJSON v
            )
    => ToPairs Plain ((n ::: v) ': nvs) (v,vr)
  where
    toPairs prb _ (v,vs) = ((T.pack $ symbolVal' (proxy# :: Proxy# n),  toJSON v) :)
                         . toPairs prb (Proxy :: Proxy nvs) vs
instance (ToPairs rep a (x,y)) => ToJSON (Proxy '(rep,a), (x,y)) where
    toJSON (_,x)
        = object
        $ toPairs (proxy# :: Proxy# rep) (Proxy :: Proxy a) x []

instance FromJSON (Proxy '(Plain,'[]), ())
  where
    parseJSON (Object v)
        | HM.null v = return (Proxy :: Proxy '(Plain,'[]), ())
        | otherwise = mzero
    parseJSON _     = mzero

instance    ( KnownSymbol n
            , FromJSON v
            , FromJSON (Proxy '(Plain, nvs), vr)
            )
            => FromJSON (Proxy '(Plain, ((n ::: v) ': nvs)), (v,vr))
  where
    parseJSON (Object hm) = do
        (_ :: Proxy '(Plain, nvs), rest::vr) <- parseJSON (Object hm')

        fmap ( (Proxy :: Proxy '(Plain, ((n ::: v) ': nvs)),)
             . (,rest)
             ) $ hm .: name
      where
        name = T.pack (symbolVal' (proxy# :: Proxy# n))
        hm' = HM.delete name hm

instance (ToJSON (Proxy '(rep,a), ar)) => ToJSON (Proxy '(rep,a), [ar])
  where
    toJSON (p,xs) = toJSON $ map (p,) xs

instance (FromJSON (Proxy '(rep,a), ar)) => FromJSON (Proxy '(rep,a), [ar])
  where
    parseJSON v
        = fmap ((Proxy :: Proxy '(rep,a),) . map snd)
            (parseJSON v :: Parser [(Proxy '(rep,a), ar)])
