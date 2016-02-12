{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedRecord2
    where

import Data.Singletons.Prelude
import Data.Promotion.Prelude.List
import GHC.TypeLits -- (Symbol, KnownSymbol, SomeSymbol(..), KnownNat, symbolVal, natVal)
import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Type.Equality -- (type (==))

import NRTH
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

{-
type family UnProxy a :: k where
    UnProxy (Proxy x) = x
-}

type family ToRep (a :: [*]) :: * where
    ToRep '[] = ()
    ToRep (x ': xs) = (x, ToRep xs)

type family FromRep (a :: *) :: [*] where
    FromRep () = '[]
    FromRep (x,y) = x ': FromRep y

type NRec (a :: [(k,*)]) = Map FstSym0 a
type VRec (a :: [(k,*)]) = Map SndSym0 a
type NVRec (a :: [k]) (b :: [*]) = Zip a b
type VRecRep (a :: [(k,*)]) = ToRep (VRec a)
-- type PVRecRep (a :: [(k,*)]) = (Proxy a, VRecRep a)
type NVRecRep (a :: [k]) (b :: *) = NVRec a (FromRep b)

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


{-
import GHC.Prim(Proxy#, proxy#)
import Data.Either(lefts)
import Data.Proxy(Proxy(..))
import GHC.TypeLits -- (Symbol, KnownSymbol, SomeSymbol(..), KnownNat, symbolVal, natVal)
import Data.Type.Bool(type (&&), type (||), If)
import Data.Type.Equality(type (==))
import Lens.Micro -- (_1, _2)
import Data.Default(Default(..))

import Data.Map(Map)
import qualified Data.Map as M
import Data.List(intercalate)
import Data.Typeable(Typeable(..))
infixr 9 :>


------------ Construction ----------------
-- | Construct Named Record type by adding fields from left to right.
--
--   It construct tree from tuples with property:
--
--   * Left branch has the same count of elements as right branch or one more
--
--   Places of elements in tree are defined by order of addition
type family (+>) a b where
    -- Minimal record
    (+>) (n1:>v1) (n2:>v2)  = (n1:>v1, n2:>v2)
    -- Adding field
    (+>) (a, b) (n:>v)      = AddB (EqCnt a b) a b (n:>v)
    -- Adding record (associativity)
    (+>) a (b,c)            = a +> Init (b,c) +> Last (b,c)
    -- Neutral element
    (+>) a () = a
    (+>) () a = a

-- type a ++> b = a +> b:>()

-- | Compare cnt of elements in tupled-tree
type family EqCnt a b :: Bool where
    EqCnt (n1:>v1) (n2:>v2) = True
    EqCnt (a,b) (n:>v) = False
    EqCnt (a,b) (c,d) = (EqCnt a c) && (EqCnt b d)

-- | The same as (+>) but with Bool parameter. Used internally
type family AddB (x::Bool) a b nv where
    AddB True a b nv = (a +> nv, b)
    AddB False a b nv = (a, b +> nv)

-- | Last added element
type family Last a where
    Last (n:>v) = n:>v
    Last (n1:>v1, n2:>v2) = n2:>v2
    Last (a,b) = LastB (EqCnt a b) a b

-- | The same as Last but with Bool parameter. Used internally
type family LastB (x::Bool) a b where
    LastB True  a b = Last b
    LastB False a b = Last a

-- | Record without last added element
type family Init a where
    Init (n:>v) = ()
    Init (n1:>v1, n2:>v2) = n1:>v1
    Init (a,b) = InitB (EqCnt a b) a b

-- | The same as Init but with Bool parameter. Used internally
type family InitB (x::Bool) a b where
    InitB True  a b = (a, Init b)
    InitB False a b = (Init a, b)

-- | Does record contain an element or another record?
type family Has a b :: Bool where
    Has (n:>v) (n:>v) = True
    Has (n:>v) (n:>()) = True
    Has (n:>v) (Proxy n) = True
    Has a (b,c) = (Has a b) && (Has a c)
    Has a (Proxy ('[] :: [Symbol])) = True
    Has a (Proxy (n ': ns :: [Symbol])) = (Has a (Proxy n)) && (Has a (Proxy ns))
    Has (a,b) (n:>v) = (Has a (n:>v)) || (Has b (n:>v))
    Has (a,b) (Proxy n) = (Has a (Proxy n)) || (Has b (Proxy n))
    Has a a = True
    Has a b = False

type family Diff a b where
    Diff () x = ()
    Diff a (Proxy '[])  = a
    Diff (n1:>v1) (n2:>v2)    = If (n1==n2 && (v1==v2 || v2 == ())) () (n1:>v1)
    Diff (n1:>v1) (Proxy n2)  = If (n1==n2) () (n1:>v1)
    Diff x (a,b) = Diff (Diff x a) b
    Diff a (Proxy (n ': ns))  = Diff (Diff a (Proxy n)) (Proxy ns)
    Diff (a,b) (n:>v) = Diff (Init (a,b)) (n:>v) +> Diff (Last (a,b)) (n:>v)
    Diff (a,b) (Proxy n) = Diff (Init (a,b)) (Proxy n) +> Diff (Last (a,b)) (Proxy n)
    -- Diff (a,b) n      = Diff (Init (a,b)) n +> Diff (Last (a,b)) n

type family Proj a b where
    Proj () x = ()
    Proj a (Proxy a)          = a
    Proj a (Proxy '[])  = ()
    Proj (n1:>v1) (n2:>v2)    = If (n1==n2 && (v1==v2 || v2 == ())) (n1:>v1) ()
    Proj (n1:>v1) (Proxy n2)  = If (n1==n2) (n1:>v1) ()
    Proj x (a,b)              = Proj x (Init (a,b)) +> Proj x (Last (a,b))
    Proj a (Proxy (n ': ns))  = Proj a (Proxy n) +> Proj a (Proxy ns)
    Proj (a,b) (n:>v)         = Proj a (n:>v) +> Proj b (n:>v)
    Proj (a,b) (Proxy n)      = Proj a (Proxy n) +> Proj b (Proxy n)

-- | Construct Named Record value by adding values
class (Has a b ~ False) => AddRec a b where
    (+>) :: a -> b -> a +> b

-- | Minimal record value
instance (Has (n1:>v1) (n2:>v2) ~ False) => AddRec (n1:>v1) (n2:>v2) where
    a +> b = (a,b)

-- | Adding next field
instance (AddRecB (EqCnt a b) a b (n :> v), Has (a,b) (n:>v) ~ False)
        => AddRec (a,b) (n:>v) where
    x +> y = add (Proxy :: Proxy (EqCnt a b)) x y

-- | The same as AddRec but with Bool parameter. Used internally
class AddRecB (x::Bool) a b nv where
    add :: Proxy x -> (a,b) -> nv -> AddB x a b nv

instance (AddRec a nv) => AddRecB True a b nv where
    add _ (x,y) nv = (x +> nv, y)

instance (AddRec b nv) => AddRecB False a b nv where
    add _ (x,y) nv = (x, y +> nv)

-- | Adding record (associativity). Like join.
instance (Has a (b,c) ~ False, RecStack (b,c)
        , AddRec a (Init (b,c)), AddRec (a +> Init (b,c)) (Last (b,c))
        ) =>
        AddRec a (b,c)
  where
    x +> y = let (i,l) = recInitLast y in x +> i +> l

-- | Record as stack (LIFO). Used for adding record (adding associativity).
-- Could be a reason for slow-down in compile-time
class RecStack a where
    recInitLast :: a -> (Init a, Last a) -- ^ Record without last added element
                                         --   and last added element.

instance RecStack (n:>v) where
    recInitLast a = ((),a)

instance RecStack (n1:>v1, n2:>v2) where
    recInitLast = id

-- | The same as RecStack but with Bool parameter. Used internally
class RecStackB (x::Bool) a b where
    recInitLastB :: Proxy x -> a -> b -> (Init (a,b), Last (a,b))

instance (RecStack b, Last (a,b) ~ Last b, Init (a,b) ~ (a, Init b)) =>
        RecStackB True a b where
    recInitLastB _ a b = let (i,l) = recInitLast b in ((a, i), l)

instance (RecStack a, Last (a,b) ~ Last a, Init (a,b) ~ (Init a, b))
        => RecStackB False a b where
    recInitLastB _ a b = let (i,l) = recInitLast a in ((i,b), l)

instance  (RecStackB (EqCnt (a,c) b) (a,c) b, RecStack (InitB (EqCnt (a, c) b) (a, c) b)) =>
        RecStack ((a,c),b) where
    recInitLast (a,b) = recInitLastB (Proxy :: Proxy (EqCnt (a,c) b)) a b

-------
{-
There can be lenses like

numLens :: Proxy (n::Nat) -> (v -> f v) -> a -> f a

but we can do it later
-}
---------- Lens -----------------------
-- | Lens for field values. Algorithm is the same as for 'RecLens'.
--   But another type for method.
class (Has a (n:>v) ~ True) => FieldLens a (n::Symbol) v where
    fieldLens :: (Functor f) => Proxy (n:>v) -> (v -> f v) -> a -> f a

-- | The same as FieldLens but with Bool parameter. Used internally
class FieldLensB a b n v (isLeft::Bool) where
    fldB :: (Functor f)
        => Proxy isLeft -> Proxy (n:>v) -> (v -> f v) -> (a,b) -> f (a,b)

instance (FieldLens a n v) => FieldLensB a b n v True where
    fldB _ p = _1 . fieldLens p

instance (FieldLens b n v) => FieldLensB a b n v False where
    fldB _ p = _2 . fieldLens p

instance FieldLens (n:>v) n v where
    fieldLens _ f (V v) = fmap V $ f v

instance (Has (a,b) (n:>v) ~ True, FieldLensB a b n v (Has a (n:>v)))
    => FieldLens (a,b) n v
  where
    fieldLens = fldB (Proxy :: Proxy (Has a (n:>v)))

-- | Lens for Named record. It is like Projection and Inclusion.
class (Has a b ~ True) => RecLens a b where
    recLens :: Lens' a b

-- | The same as RecLens but with Bool parameter. Used internally
class RecLensB a b c (isLeft::Bool) where
    recB :: Proxy isLeft -> Lens' (a,b) c

instance (RecLens a с) => RecLensB a b с True where
    recB _ = _1 . recLens

instance (RecLens b с) => RecLensB a b с False where
    recB _ = _2 . recLens

instance RecLens (n:>v) (n:>v) where
    recLens = id

instance ((Has a (n:>v) || Has b (n:>v)) ~ True, RecLensB a b (n:>v) (Has a (n:>v)))
    => RecLens (a,b) (n:>v)
  where
    recLens = recB (Proxy :: Proxy (Has a (n:>v)))

instance ((Has a b && Has a c) ~ True, RecLens a b, RecLens a c)
    => RecLens a (b,c)
  where
    recLens = lens  ((,) <$> (^. recLens) <*> (^. recLens))
                    (\x (v1,v2) -> x & recLens .~ v1 & recLens .~ v2)

--------- Initialization, Conversion ----------------
-- | We need interface to make strong-typed Named Record from weak-typed generalized struct.
--
-- So there is type 'Lifted' where field values lifted in some functor.
-- Common case is @Lifted Maybe@. Then we have 'Default' instance for @Lifted Maybe T@.
-- And then fill it using Lenses.
type family Lifted f a where
    Lifted f (n:>v) = n :> (f v)
    Lifted f (a,b) = (Lifted f a, Lifted f b)

-- | Conversion from @Lifted Maybe a@ to @a@.
--
-- If some field values is not initialized
-- than for optional fields they will have empty value.
-- If there is uninitialized required field than conversion is impossible and
-- we'll got list of these fields.
--
-- Optional fields have type @Maybe a@ or @[a]@ but not @String@
class ToRec a where
    toRec :: Lifted Maybe a -> Either [SomeSymbol] a

instance (KnownSymbol n, ToRecDef (HasDef v) v) => ToRec (n:>v) where
    toRec (V mv)
        = maybe ( maybe
                    (Left [SomeSymbol (Proxy::Proxy n)])
                    (Right . V)
                    $ toRecDef (proxy# :: Proxy# (HasDef v))
                )
                (Right . V)
                mv

instance (ToRec a, ToRec b) => ToRec (a,b) where
    toRec (a,b) = case (toRec a, toRec b) of
        (Right x, Right y) -> Right (x,y)
        (x, y) -> Left $ concat $ lefts [x] ++ lefts [y]

class ToRecDef (b::Bool) a where
    toRecDef :: Proxy# b -> Maybe a

instance (Default a) => ToRecDef True a where
    toRecDef _ = Just def

instance ToRecDef False a where
    toRecDef _ = Nothing

-- | Get names and values as diff-function @[String] -> [String]@.
-- An order is not correspond to original but the same for names and for values
class NamesList a where
    toNames :: Proxy# a -> [SomeSymbol] -> [SomeSymbol]
    toNamesStrL :: Proxy# a -> [String] -> [String]

class ValuesList a where
    toValues :: a -> [String] -> [String]

-- | List of names.
-- An order is correspond to original
names :: (NamesList a) => Proxy# a -> [SomeSymbol]
names p = toNames p []

namesStr :: (NamesList a) => Proxy# a -> String
namesStr p = intercalate "," (namesStrL p)

namesStrL :: (NamesList a) => Proxy# a -> [String]
namesStrL p = toNamesStrL p []

-- | List of values
-- An order is correspond to original
values :: (ValuesList a) => a -> [String]
values x = toValues x []

instance (KnownSymbol n) => NamesList (n:>v) where
    toNames _       = (SomeSymbol (Proxy :: Proxy n) :)
    toNamesStrL _   = (symbolVal' (proxy# :: Proxy# n) :)

instance (KnownSymbol n) => NamesList (Proxy n) where
    toNames _       = (SomeSymbol (Proxy :: Proxy n) :)
    toNamesStrL _   = (symbolVal' (proxy# :: Proxy# n) :)

instance (Show v) => ValuesList (n:>v) where
    toValues (V x) = (show x :)

instance (RecStack (x,y), NamesList (Last (x,y)), NamesList (Init (x,y)))
    => NamesList (x,y) where
    toNames _       = toNames (proxy# :: Proxy# (Init (x,y)))
                    . toNames (proxy# :: Proxy# (Last (x,y)))
    toNamesStrL _   = toNamesStrL (proxy# :: Proxy# (Init (x,y)))
                    . toNamesStrL (proxy# :: Proxy# (Last (x,y)))

instance NamesList (Proxy '[]) where
    toNames _       = id
    toNamesStrL _   = id

instance (NamesList (Proxy n), NamesList (Proxy ns))
    => NamesList (Proxy (n ': ns)) where
    toNames _       = toNames (proxy# :: Proxy# (Proxy n))
                    . toNames (proxy# :: Proxy# (Proxy ns))
    toNamesStrL _   = toNamesStrL (proxy# :: Proxy# (Proxy n))
                    . toNamesStrL (proxy# :: Proxy# (Proxy ns))

instance (RecStack (x,y), ValuesList (Last (x,y)), ValuesList (Init (x,y)))
    => ValuesList (x,y) where
    toValues (x,y) = let (i,l) = recInitLast (x,y) in toValues i . toValues l
{-
type family Foo (a::*) (b::*) :: Bool where
    --Foo a a = True
    --Foo a b = False
    Foo a b = a == b

foo :: Foo a b ~ True => Proxy a -> Proxy b
foo _ = Proxy

bar :: Proxy a -> Proxy a
bar = foo
-}
-}
