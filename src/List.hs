{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DataKinds #-}

module List(ListCons(..)) where

import GHC.TypeLits
import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality
import Fields ((:>)(..))

infixr 6 +>

class ListCons a b where
    type a +> b
    (+>) :: a -> b -> a +> b

instance ListCons ((s::Symbol):>a) () where
    type (s:>a) +> () = (s:>a,())
    (+>) = (,)

instance (ListConsC (True{-CmpSymbol sa sb == LT-}) (sa:>a) (sb:>b,c))
    => ListCons (sa:>a) (sb:>b,c)
  where
    type (sa:>a) +> (sb:>b,c)
        = TLC (True{-CmpSymbol sa sb == LT-}) (sa:>a) (sb:>b,c)
    (+>) = lcc (Proxy :: Proxy (True{-CmpSymbol sa sb == LT-}))

class ListConsC (o::Bool) a b where
    type TLC o a b
    lcc :: Proxy o -> a -> b -> TLC o a b

instance ListConsC True a b where
    type TLC True a b = (a,b)
    lcc _ a b = (a,b)

instance (ListCons a c) => ListConsC False a (b,c) where
    type TLC False a (b,c) = (b, a +> c)
    lcc _ a (b,c) = (b,a+>c)

{-
type T1  = "1" :>Int +> ()
type T2  = "2" :>Int +> T1
type T3  = "3" :>Int +> T2
type T4  = "4" :>Int +> T3
type T5  = "5" :>Int +> T4
type T6  = "6" :>Int +> T5
type T7  = "7" :>Int +> T6
type T8  = "8" :>Int +> T7
type T9  = "9" :>Int +> T8
type T10 = "10":>Int +> T9
type T11 = "11":>Int +> T10
type T12 = "12":>Int +> T11
type T13 = "13":>Int +> T12
type T14 = "14":>Int +> T13
type T15 = "15":>Int +> T14
type T16 = "16":>Int +> T15
type T17 = "17":>Int +> T16
type T18 = "18":>Int +> T17
type T19 = "19":>Int +> T18
type T20 = "20":>Int +> T19
type T21 = "21":>Int +> T20
type T22 = "22":>Int +> T21
type T23 = "23":>Int +> T22
type T24 = "24":>Int +> T23

t1  = (V 1  :: "1"  :> Int) +> ()
t2  = (V 2  :: "2"  :> Int) +> t1
t3  = (V 3  :: "3"  :> Int) +> t2
t4  = (V 4  :: "4"  :> Int) +> t3
t5  = (V 5  :: "5"  :> Int) +> t4
t6  = (V 6  :: "6"  :> Int) +> t5
t7  = (V 7  :: "7"  :> Int) +> t6
t8  = (V 8  :: "8"  :> Int) +> t7
t9  = (V 9  :: "9"  :> Int) +> t8
t10 = (V 10 :: "10" :> Int) +> t9
t11 = (V 11 :: "11" :> Int) +> t10
t12 = (V 12 :: "12" :> Int) +> t11 :: T12

-}
