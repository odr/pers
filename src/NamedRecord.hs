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
    , EqCnt
    , RecLens(..)
    , FieldLens(..)
    , Has
    , Lifted
    , ToRec(..)
    , Last
    , Init
    , RecStack(..)
    , ToRecDef(..)
    , ValuesList(..)
    , NamesList(..)
    , names
    , namesStr
    , values
    , HasDef
    ) where

import Data.Either(lefts)
import Data.Proxy(Proxy(..))
import GHC.TypeLits(Symbol, KnownSymbol, SomeSymbol(..), symbolVal)
import Data.Type.Bool(type (&&), type (||))
import Lens.Micro -- (_1, _2)
import Data.Default(Default(..))

import Data.Map(Map)
import qualified Data.Map as M
import Data.List(intercalate)


import NamedValue -- ((:>)(..), valLens)

infixl 6 +>

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
    (+>) (a, b) (n:>v)      = Add (EqCnt a b) a b (n:>v)
    -- Adding record (associativity)
    (+>) a (b,c)            = a +> Init (b,c) +> Last (b,c)

-- | Compare cnt of elements in tupled-tree
type family EqCnt a b :: Bool where
    EqCnt (n1:>v1) (n2:>v2) = True
    EqCnt (a,b) (n:>v) = False
    EqCnt (a,b) (c,d) = (EqCnt a c) && (EqCnt b d)

-- | The same as (+>) but with Bool parameter. Used internally
type family Add (x::Bool) a b nv where
    Add True a b nv = (a +> nv, b)
    Add False a b nv = (a, b +> nv)

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
    add :: Proxy x -> (a,b) -> nv -> Add x a b nv

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
    --recLast :: a -> Last a -- ^ Last added element
    --recInit :: a -> Init a -- ^ Record without last added element
    recInitLast :: a -> (Init a, Last a) -- ^ Record without last added element
                                         --   and last added element.


instance RecStack (n:>v) where
    --recLast = id
    --recInit _ = ()
    recInitLast a = ((),a)

instance RecStack (n1:>v1, n2:>v2) where
    --recLast = snd
    --recInit = fst
    recInitLast = id

-- | The same as RecStack but with Bool parameter. Used internally
class RecStackB (x::Bool) a b where
    --recLastB :: Proxy x -> a -> b -> Last (a,b)
    --recInitB :: Proxy x -> a -> b -> Init (a,b)
    recInitLastB :: Proxy x -> a -> b -> (Init (a,b), Last (a,b))

instance (RecStack b, Last (a,b) ~ Last b, Init (a,b) ~ (a, Init b)) =>
        RecStackB True a b where
    --recLastB _ a b = recLast b
    --recInitB _ a b = (a, recInit b)
    recInitLastB _ a b = let (i,l) = recInitLast b in ((a, i), l)

instance (RecStack a, Last (a,b) ~ Last a, Init (a,b) ~ (Init a, b))
        => RecStackB False a b where
    --recLastB _ a b = recLast a
    --recInitB _ a b = (recInit a, b)
    recInitLastB _ a b = let (i,l) = recInitLast a in ((i,b), l)

instance  (RecStackB (EqCnt (a,c) b) (a,c) b, RecStack (InitB (EqCnt (a, c) b) (a, c) b)) =>
        RecStack ((a,c),b) where
    --recLast (a,b) = recLastB (Proxy :: Proxy (EqCnt (a,c) b)) a b
    --recInit (a,b) = recInitB (Proxy :: Proxy (EqCnt (a,c) b)) a b
    recInitLast (a,b) = recInitLastB (Proxy :: Proxy (EqCnt (a,c) b)) a b

---------- Lens -----------------------
-- | Does record contain an element or another record?
type family Has a b :: Bool where
    Has (n:>v) (n:>v) = True
    Has (a,b) (n:>v) = (Has a (n:>v)) || (Has b (n:>v))
    Has a (b,c) = (Has a b) && (Has a c)
    Has a b = False

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
                    $ toRecDef (Proxy :: Proxy (HasDef v))
                )
                (Right . V)
                mv

instance (ToRec a, ToRec b) => ToRec (a,b) where
    toRec (a,b) = case (toRec a, toRec b) of
        (Right x, Right y) -> Right (x,y)
        (x, y) -> Left $ concat $ lefts [x] ++ lefts [y]

-- | Does this type can have default (null) value.
--   True for Maybe and [] but not for String.
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

-- | Get names and values as diff-function @[String] -> [String]@.
-- An order is not correspond to original but the same for names and for values
class NamesList a where
    toNames :: Proxy a -> [SomeSymbol] -> [SomeSymbol]

class ValuesList a where
    toValues :: a -> [String] -> [String]

-- | List of names.
-- An order is not correspond to original but the same for names and for values
names :: (NamesList a) => Proxy a -> [SomeSymbol]
names p = toNames p []

namesStr :: (NamesList a) => Proxy a -> String
namesStr = intercalate "," . map (\(SomeSymbol n)-> symbolVal n) . names

-- | List of values
-- An order is not correspond to original but the same for names and for values
values :: (ValuesList a) => a -> [String]
values x = toValues x []

instance (KnownSymbol n) => NamesList (n:>v) where
    toNames _ = (SomeSymbol (Proxy :: Proxy n) :)

instance (Show v) => ValuesList (n:>v) where
    toValues (V x) = (show x :)

instance (NamesList a, NamesList b) => NamesList (a,b) where
    toNames _ = toNames (Proxy :: Proxy a) . toNames (Proxy :: Proxy b)

instance (ValuesList a, ValuesList b) => ValuesList (a,b) where
    toValues (x,y) = toValues x . toValues y



