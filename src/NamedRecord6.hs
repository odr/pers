{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module NamedRecord6 where

import Data.Typeable(Proxy(..), Typeable(..))
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Either(either, lefts)
import Control.Lens
import Data.Default(Default(..))

infixr 9 :>
infixl 6 +>

newtype s :> val = V val deriving (Typeable, Show, Eq, Ord, Functor, Traversable, Foldable)
instance (Default val) => Default (s:>val) where
    def = V def

type family (+>) a b where
    (+>) (n1:>v1) (n2:>v2)
        = (n1:>v1, n2:>v2)
    (+>)  (a, b) (n:>v)
        = Add (EqCnt a b) (a, b) (n:>v)
        -- = Add (Cnt a == Cnt b) (a, b) (n:>v)

type family EqCnt a b :: Bool where
    EqCnt (n1:>v1) (n2:>v2) = True
    EqCnt (a,b) (n:>v) = False
    EqCnt (a,b) (c,d) = (EqCnt a c) && (EqCnt b d)

type family Add (x::Bool) a b where
    Add True (a,b) (n:>v) = (a +> n:>v, b)
    Add False (a,b) (n:>v) = (a, b +> n:>v)

{-
type family Cnt a :: Nat where
    Cnt (n:>v) = 1
    Cnt (a,b) = Cnt a + Cnt b
-}

type family Lifted f a where
    Lifted f (n:>v) = n :> (f v)
    Lifted f (a,b) = (Lifted f a, Lifted f b)

type family Has a (n :: Symbol) v :: Bool where
    Has (n:>v) n v = True
    Has (a,b) n v = (Has a n v) || (Has b n v)
    Has a n v = False

class (Has a n v ~ True) => Field a (n::Symbol) v where
    fld :: (Functor f) => Proxy (n:>v) -> (v -> f v) -> a -> f a

class FieldB a b n v (isLeft::Bool) where
    fldB :: (Functor f)
        => Proxy isLeft -> Proxy (n:>v) -> (v -> f v) -> (a,b) -> f (a,b)

instance (Field a n v) => FieldB a b n v True where
    fldB _ p = _1 . fld p

instance (Field b n v) => FieldB a b n v False where
    fldB _ p = _2 . fld p

instance Field (n:>v) n v where
    fld _ f (V v) = fmap V $ f v

instance ((Has a n v || Has b n v) ~ True, FieldB a b n v (Has a n v))
    => Field (a,b) n v
  where
    fld = fldB (Proxy :: Proxy (Has a n v))


---------------------------------

type T = "0":>Int +> "1":>Char +> "2":>String

p = Proxy :: Proxy T

h :: Proxy (n:>v) -> Proxy (Has T n v)
h _ = Proxy

x = ((V 1, V "x"),V 'y') :: T

type Person = "name":>String
            +> "age":>Int
            +> "gender":>Bool
            +> "year":>Int
            +> "month":>Int
            +> "day":>Int
            +> "week":>Maybe Int
            +> "1":>Int
            +> "2":>Int
            +> "3":>Int
            +> "4":>Int
            +> "5":>Int
            +> "6":>Int
            +> "7":>Int
            +> "8":>Int
            +> "9":>Int
            +> "10":>Int
            +> "11":>Int
            +> "12":>Int
            +> "13":>Int
            +> "14":>Int
            +> "15":>Int
            +> "16":>Int
            +> "17":>Int
            +> "18":>Int
            +> "19":>Int
            +> "20":>Int
            +> "21":>Int
            +> "22":>Int
            +> "23":>Int
            +> "24":>Int
            +> "25":>Int
            +> "26":>Int
            +> "27":>Int
            +> "28":>Int
            +> "29":>Int
            +> "30":>Int
            +> "31":>Int
            +> "32":>Int
            +> "33":>Int
            +> "34":>Int
            +> "35":>Int
            +> "36":>Int
            +> "37":>Int
            +> "38":>Int
            +> "39":>Int
            +> "40":>Int
            +> "41":>Int
            +> "42":>Int
            +> "43":>Int
            +> "44":>Int
            +> "45":>Int
            +> "46":>Int
            +> "47":>Int
            +> "48":>Int
            +> "49":>Int

defPerson = def :: Lifted Maybe Person  -- 8s
defPerson2 = def :: Lifted Maybe Person -- 11s
b = defPerson == defPerson2
-- 30s (with If) -> 11,5s (without If) -> 10,5 (Cnt -> EqCnt) -> 13s (Bool::+Clear)
{-
-}


