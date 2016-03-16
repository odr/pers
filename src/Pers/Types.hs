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
-- {-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Pers.Types
    where

import GHC.TypeLits -- (Symbol, KnownSymbol, symbolVal', SomeSymbol(..), Nat)
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality(type (==))
import Data.Type.Bool(type (&&))
import Lens.Micro(Lens', (^.), (.~), (&), lens)
import GHC.Exts(Constraint)
import qualified Data.Text as T
import Data.Aeson(ToJSON(..),FromJSON(..),Value(..)
                ,fromJSON, Result(..), object, (.:))
import Data.Aeson.Types(Parser)
import Control.Monad(mzero)
import qualified Data.HashMap.Strict as HM
import Data.Tagged
import Data.Maybe(maybeToList, listToMaybe)
-- import Data.Promotion.Prelude.List(type (:++))
-- import Data.Singletons.Prelude
-- import Lucid

-- | Пара Описание и Тип-значение
type (:::) (a :: k1) (b :: k2) = '(a,b)
infixl 9 :::

-- | Kind for type of representation. Constructors are types.
data R
    = Plain -- ^ Type to define simple record representation as tuple (a,).(b,).(c,)....(z,) $ ()
proxyPlain = Proxy :: Proxy Plain

class Curring (r::R) (ts :: *) where
    type Curried r ts c
    curryN :: Proxy r -> (ts -> c) -> Curried r ts c
    uncurryN :: Proxy r -> Curried r ts c -> ts -> c

instance Curring Plain (t,()) where
    type Curried Plain (t,()) c = t -> c
    curryN _ f t = f (t,())
    uncurryN _ f (t,()) = f t

instance Curring Plain (t2,ts) => Curring Plain (t1,(t2,ts)) where
    type Curried Plain (t1,(t2,ts)) c = t1 -> Curried Plain (t2,ts) c
    -- type UnCur Plain (t1 ': t2 ': ts) = (t1, UnCur Plain (t2 ': ts))
    curryN _ f = curryN proxyPlain . curry f
    uncurryN _ f (t1,ts) = uncurryN proxyPlain (f t1) ts

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
    VRec Plain '[]              = ()
    VRec Plain '[ '(a,b)]       = (b,())
    VRec Plain ('(a,b) ': xs)   = (b, VRec Plain xs)

type family VRec' (a :: [(k,*)]) :: [*] where
    VRec' '[]       = '[]
    VRec' ('(a,b) ': xs) = b ': VRec' xs

type family VRec'' (rep :: R) (a :: *) :: [*] where
    VRec'' Plain () = '[]
    VRec'' Plain (a,b) = a ': VRec'' Plain b

-------------------- Lenses --------------------------------
class (Rep rep b br, Rep rep a ar)
    => RecLens rep b a br ar    | rep b -> br
                                , rep a -> ar
  where
    recLens :: Proxy# '(rep,b,a) -> Lens' br ar

instance    ( Rep rep (b ': bs) br
            , Rep rep '[a] ar
            , RecLensB (a==b) rep (b ': bs) '[a] br ar
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

recLens' :: ( Rep rep b br
            , Rep rep (ProjNames b a) ar
            , RecLens rep b (ProjNames b a) br ar
            )
    => Proxy '(rep,b,a) -> Lens' br ar
recLens' (_:: Proxy '(rep,b,a))
    = recLens (proxy# :: Proxy# '(rep, b, ProjNames b a))

-----------------------------------------------

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

type family Contain (a::[k]) (b::k) :: Constraint where
    Contain (a ': as) a = ()
    Contain (a ': as) b = Contain as b

type family Contains (a::[k]) (b::[k]) :: Constraint where
    Contains as '[] = ()
    Contains as (b1 ': bs) = (Contain as b1,  Contains as bs)

type family ContainFst (a::[(k,k1)]) (b::(k,k2)) :: Constraint where
    ContainFst ('(a,x) ': as) '(a,y) = ()
    ContainFst ('(a,x) ': as) '(b,y) = ContainFst as '(b,y)

type family ContainsFst (a::[(k,k1)]) (b::[(k,k2)]) :: Constraint where
    ContainsFst as '[] = ()
    ContainsFst as (b1 ': bs) = (ContainFst as b1,  ContainsFst as bs)

type family ContainNamess (a :: [(k,k2)]) (b :: [[k]]) :: Constraint where
    ContainNamess as '[] = ()
    ContainNamess as (b ': bs) = (ContainNames as b, ContainNamess as bs)

type family MinusNames (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    MinusNames xs '[] = xs
    MinusNames ( '(a,b) ': xs ) '[a] = xs
    MinusNames ( '(a,b) ': xs ) '[c] = '(a,b) ': MinusNames xs '[c]
    MinusNames xs (y ': ys) = MinusNames (MinusNames xs '[y]) ys

type family ProjNames  (a :: [(k,*)]) (b :: [k]) :: [(k,*)] where
    ProjNames xs '[] = '[]
    ProjNames xs '[c] = '[ProjName xs c]
    ProjNames xs (y ': ys) = ProjName xs y ': ProjNames xs ys

type family (:++) (a::[k]) (b::[k]) ::[k] where
    '[] :++ a       = a
    (a ': as) :++ b = a ': (as :++ b)

type DataKeyDef a pk = MinusNames a pk :++ ProjNames a pk

type family ProjName  (a :: [(k,*)]) (b :: k) where
    ProjName ( '(a,b) ': xs ) a = '(a,b)
    ProjName ( '(a,b) ': xs ) c = ProjName xs c

------------ Names ---------------
type family CheckList (f :: k -> Constraint) (x :: [k]) :: Constraint where
    CheckList f '[] = ()
    CheckList f (x ': xs) = (f x, CheckList f xs)

class Names (x :: [Symbol]) where
    symbols :: Proxy# x -> [SomeSymbol]
    names   :: Proxy# x -> [String]
instance Names '[] where
    symbols _ = []
    names _ = []
instance (KnownSymbol s, Names ss) => Names (s ': ss) where
    symbols _ = SomeSymbol (Proxy :: Proxy s) : symbols (proxy# :: Proxy# ss)
    names _ = symbolVal' (proxy# :: Proxy# s) : names (proxy# :: Proxy# ss)
class Namess (x :: [[Symbol]]) where
    namess   :: Proxy# x -> [[String]]
instance Namess '[] where
    namess _ = []
instance (Names s, Namess ss) => Namess (s ': ss) where
    namess _ = names (proxy# :: Proxy# s) : namess (proxy# :: Proxy# ss)

------------------ JSON -------------------------
class ToPairs (ra::k) ar | ra -> ar
  where
    toPairs :: Tagged ra ar -> [(T.Text, Value)] -> [(T.Text, Value)]

instance ToPairs '(Plain,'[]) () where
    toPairs _ = id

instance    ( KnownSymbol n
            , ToPairs '(Plain,nvs) vr
            , ToJSON v
            )
    => ToPairs '(Plain, (n:::v) ': nvs) (v,vr)
  where
    toPairs (Tagged (v,vs))
        = ((T.pack $ symbolVal' (proxy# :: Proxy# n),  toJSON v) :)
        . toPairs (Tagged vs :: Tagged '(Plain, nvs) vr)

instance FromJSON (Tagged '(Plain,'[]) ())
  where
    parseJSON (Object v)
        | HM.null v = return (Tagged () :: Tagged '(Plain,'[]) ())
        | otherwise = mzero
    parseJSON _     = mzero

instance    ( KnownSymbol n
            , FromJSON v
            , FromJSON (Tagged '(Plain, nvs) vr)
            )
            => FromJSON (Tagged '(Plain, ((n ::: v) ': nvs)) (v,vr))
  where
    parseJSON (Object hm) = do
        (Tagged rest :: Tagged '(Plain, nvs) vr) <- parseJSON (Object hm')

        -- fmap ( (Proxy :: Proxy '(Plain, ((n ::: v) ': nvs)),)
        fmap (Tagged . (,rest)) $ hm .: name
      where
        name = T.pack (symbolVal' (proxy# :: Proxy# n))
        hm' = HM.delete name hm

-- (x,y) чтобы не перекрываться с [x]
instance ToPairs '(Plain, a) (x,y) => ToJSON (Tagged '(Plain,a) (x,y)) where
    toJSON = object . flip toPairs []
instance  ToJSON (Tagged '(Plain,a) (x,y))
        => ToJSON (Tagged '(Plain,b,a,ar,kr,dr) (x,y)) where
    toJSON = toJSON
        . (retag :: Tagged '(Plain,b,a,ar,kr,dr) (x,y)
                    -> Tagged '(Plain,a) (x,y)
          )

-- instance (ToJSON x) => ToJSON (Tagged r x) where
--     toJSON = toJSON . untag
-- instance (FromJSON x) => FromJSON (Tagged r x) where
--     parseJSON = fmap Tagged . parseJSON
-- instance (ToJSON (Proxy x, y)) => ToJSON (Proxy x, Tagged r y) where
--     toJSON = toJSON . fmap untag
-- instance (FromJSON (Proxy x, y)) => FromJSON (Proxy x, Tagged r y) where
--     parseJSON = fmap (fmap Tagged) . parseJSON

instance ToJSON (Tagged '(Plain,b,a,ar,kr,dr) ar) => ToJSON (Tagged '(Plain,b,a,ar,kr,dr) [ar])
  where
    toJSON = toJSON . sequence
instance ToJSON (Tagged '(Plain,a) ar) => ToJSON (Tagged '(Plain,a) [ar])
  where
    toJSON = toJSON . sequence

instance FromJSON (Tagged '(Plain,a) ar) => FromJSON (Tagged '(Plain,a) [ar])
  where
    parseJSON v = fmap sequence (parseJSON v :: Parser [(Tagged '(Plain,a) ar)])

instance ToJSON (Tagged '(Plain,a) [ar]) => ToJSON (Tagged '(Plain,a) (Maybe ar))
  where
    toJSON = toJSON . fmap maybeToList

instance FromJSON (Tagged '(Plain,a) ar) => FromJSON (Tagged '(Plain,a) (Maybe ar))
  where
    parseJSON v
        = fmap (retag . fmap listToMaybe)
            (parseJSON v :: Parser (Tagged '(Plain,a) [ar]))
--------------------------------------

-- | Does this type can have default (null) value.
--   True for Maybe and [] but not for String.
type family HasDef a :: Bool where
    HasDef (Maybe a) = True
    HasDef String = False
    HasDef [a] = True
    HasDef (a,b) = HasDef a && HasDef b
    HasDef () = True
    HasDef a = False

