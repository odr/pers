{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE OverlappingInstances #-}

module Fields
    ( HasField(..)
    , Projection(..)
    ) where
import Data.Typeable
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

newtype (s :: Symbol) :> val = V val deriving (Typeable, Show)
infixr 9 :>

class HasField name val a | a name -> val where
    get :: Proxy name -> a -> val
    set :: Proxy name -> val -> a -> a

class HasFieldB b name val a | a name -> val where
    getB :: Proxy b -> Proxy name -> a -> val
    setB :: Proxy b -> Proxy name -> val -> a -> a

class Projection a b where
    proj :: a -> b

-- HasFieldB ------

-- trio
instance HasFieldB b n v (l, n:>v, r)
  where
    getB _ _ (_, V x, _) = x
    setB _ _ y (l, V x, r) = (l, V y, r)

instance (HasField name v r)
    => HasFieldB GT name v (l, n :> v1, r)
  where
    getB _ pn (_,_,r) = get pn r
    setB _ pn v (l,x,r) = (l, x, set pn v r)

instance (HasField name v l)
    => HasFieldB LT name v (l, n :> v1, r)
  where
    getB _ pn (l,_,_) = get pn l
    setB _ pn v (l,x,r) = (set pn v l, x, r)

-- HasField ---

-- singleton
instance HasField name val (name :> val)
  where
    get _ (V x) = x
    set _ y (V x) = V y

-- pairs
instance HasField n v (n1:>v1, n:>v)
  where
    get _ (_, V x) = x
    set _ y (l, V x) = (l, V y)

instance HasField n v (n:>v, n1:>v1)
  where
    get _ (V x, _) = x
    set _ y (V x, r) = (V y, r)

-- trio
instance (HasFieldB (CmpSymbol n n0) n v (l,n0:>v0,r))
    => HasField n v (l,n0:>v0,r)
  where
    get = getB (Proxy :: Proxy (CmpSymbol n n0))
    set = setB (Proxy :: Proxy (CmpSymbol n n0))

-- Projection ---

-- singleton
instance (HasField n v a) => Projection a (n :> v)
  where
    proj = V . get (Proxy :: Proxy n)

-- pair
instance (HasField n1 v1 a, HasField n2 v2 a)
    => Projection a (n1 :> v1, n2 :> v2)
  where
    proj = (,)  <$> V . get (Proxy :: Proxy n1)
                <*> V . get (Proxy :: Proxy n2)

-- trio
instance (Projection a l, HasField n v a, Projection a r)
    => Projection a (l, n :> v, r)
  where
    proj a = (proj a :: l, V $ get (Proxy :: Proxy n) a, proj a :: r)


{-
If we want to have a balanced tree then records are hard-extensible.
It means that creation of record is also problematic.
I see two ways to make records extensible:
1. Provide conversion between Symbol and Nat for datatype.
    Then use order by Nat instead of Symbol. The top-most item will have type 1.
2. Use unbalanced but ordered tree.
    In this case top item determined by first three Symbols added

Perhaps one can make tree balanced by adding weight::Nat for the left and right side of the tree?
-}
{-
class NamedExt n v a b | n v a -> b, a b -> n v where -- , n v b -> a where
    (+>) :: n :> v -> a -> b
infixr 6 +>

class NamedExtB o n v a b | n v a -> b, a b -> n v where
    nbInc :: Proxy o -> n :> v -> a -> b

{-
instance NamedExt n v () (n:>v) where
    x +> () = x
-}

instance (CmpSymbol n1 n2 ~ LT)
    => NamedExt n1 v1 (n2:>v2) (n1:>v1,n2:>v2)
  where
    (+>) = (,)

instance (CmpSymbol n1 n2 ~ GT)
    => NamedExt n1 v1 (n2:>v2) (n2:>v2,n1:>v1)
  where
    (+>) = flip (,)

-- pair
instance (CmpSymbol n n1 ~ LT)
    => NamedExt n v (n1 :> v1, x) (n :> v, n1 :> v1, x)
  where
    a +> (b, c) = (a, b, c)

instance (CmpSymbol n n1 ~ GT, CmpSymbol n n2 ~ LT)
    => NamedExt n v (n1 :> v1, n2 :> v2) (n1 :> v1, n :> v, n2 :> v2)
  where
    a +> (b, c) = (b, a, c)

instance (CmpSymbol n n2 ~ GT)
    => NamedExt n v (x, n2 :> v2) (x, n2 :> v2, n :> v)
  where
    a +> (b, c) = (b, c, a)

-- trio
{-
instance (CmpSymbol n nt ~ LT, NamedExt n v x z)
    => NamedExt n v (x, nt :> vt, y) (z, nt :> vt, y)
  where
    a +> (b, c, d) = (a +> b, c, d)

instance (CmpSymbol n nt ~ GT, NamedExt n v y z)
    => NamedExt n v (x, nt :> vt, y) (x, nt :> vt, z)
  where
    a +> (b, c, d) = (b, c, a +> d)
-}
instance (NamedExtB (CmpSymbol n n0) n v (l,n0:>v0,r) b)
    => NamedExt n v (l,n0:>v0,r) b
  where
    a +> b = nbInc (Proxy :: Proxy (CmpSymbol n n0)) a b

--- Test ---
type Test = ((("a" :> Int, "b" :> String), "c" :> Int, "d" :> Char), "e" :> Double
            , ("f" :> Integer, "g" :> [Int], "h" :> (Int, Char)))

test = (((V 1, V "2b"), V 3, V '4'), V 5.1, (V 6, V [7,7], V (8,'h'))) :: Test

type TestP = ((("a" :> Int, "b" :> String), "c" :> Int, "d" :> Char), "e" :> Double
            , ("g" :> [Int], "h" :> (Int, Char)))
testP = proj test :: TestP

type TestNP = ((("a" :> Int, "b" :> String), "c" :> Integer, "d" :> Char), "e" :> Double
            , ("g" :> [Int], "h" :> (Int, Char)))

r1 = V 'a' :: "a5":>Char
r2 = (V "cc" :: "c5":>String) +> r1 -- :: ("a5":>Char,"c5":>String)
r3 = V ["b","bb"] +> r2 :: ("a5":>Char, "b5":>[String], "c5":>String)
-- r4 = V ["a","1"] +> r3 :: (("a1":>[String],"a5":>Char), "b5":>[String], "c5":>String)
-- testNP = proj test :: TestNP -- not compiled

pa = Proxy :: Proxy "a"
pb = Proxy :: Proxy "b"
pc = Proxy :: Proxy "c"
pd = Proxy :: Proxy "d"
pe = Proxy :: Proxy "e"
pf = Proxy :: Proxy "f"
pg = Proxy :: Proxy "g"
ph = Proxy :: Proxy "h"

-}
