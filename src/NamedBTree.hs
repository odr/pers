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
{-# LANGUAGE RankNTypes #-}

module NamedBTree
    -- ( (<+)
    -- , (<\)
    ( NTAdd(..)
    , NTDel(..)
    , NTMin(..)
    , NTMax(..)
    , TName
    , TMin
    , TMax
    , type (<+)
    , type (<\)
    -- , (:>)(..)
    ) where
import Data.Typeable
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Fields((:>)(..))

infixl 6 <+
infixl 6 <\

-- interface: classes -----
class NTAdd a d b | a d -> b where
    (<+) :: a -> d -> b

class NTDel a n b | a n -> b where
    (<\) :: a -> Proxy n -> b

class NTMin a nv | a -> nv where
    getNTMin :: a -> nv

class NTMax a nv | a -> nv where
    getNTMax :: a -> nv

type family TName a where
    TName (n:>v) = n

type family IsEven (k::Nat) :: Bool where
    IsEven 0 = True
    IsEven 1 = False
    IsEven 2 = True
    IsEven 3 = False
    IsEven 4 = True
    IsEven 5 = False
    IsEven 6 = True
    IsEven 7 = False
    IsEven k = IsEven (k-8)

type family TMin a where
    TMin (1:>(0:>(),b,c))   = b
    TMin (k:>(a,b,c))       = TMin a

type family TMax a where
    TMax (k:>(a,b,0:>()))   = b
    TMax (k:>(a,b,c))       = TMax c

type family a <+ b where
    () <+ (n:>v) = (1:>(0:>(),n:>v,0:>()))
    (0:>()) <+ (n:>v) = (1:>(0:>(),n:>v,0:>()))
    (k:>(kl:>l,nt:>vt,kr:>r)) <+ (n:>v)
        = (k+1) :> If (CmpNat kl kr == GT)
            -- grow right
            (If (CmpSymbol n nt == GT)
                -- add right 4
                (kl:>l,nt:>vt,kr:>r <+ n:>v)
                -- add left 3
                (kl:>l <+ n:>v <\ TName (TMax (kl:>l <+ n:>v)), TMax (kl:>l <+ n:>v), kr:>r <+ nt:>vt)
            )
            -- grow left
            (If (CmpSymbol n nt == GT)
                -- add right 2
                (kl:>l <+ nt:>vt, TMin (kr:>r <+ n:>v), kr:>r <+ n:>v <\ TName (TMin (kr:>r <+ n:>v)))
                -- add left 1
                (kl:>l <+ n:>v, nt:>vt, kr:>r)
            )
type family (a :: *) <\ (b :: Symbol) :: * where
    (1:>(0:>(),n:>v,0:>())) <\ n = 0:>()
    (k:>(kl:>l,nt:>vt,kr:>r)) <\ n
        = (k-1) :> If (CmpNat kr kl == LT)
            -- reduce left
            (If (CmpSymbol n nt == GT)
                -- del right 3
                (kl:>l <\ TName (TMax (kl:>l)), TMax (kl:>l), kr:>r <\ n <+ nt:>vt)
                (If (CmpSymbol n nt == EQ)
                    -- del top 2
                    (kl:>l <\ TName (TMax (kl:>l)), TMax (kl:>l), kr:>r)
                    -- del left 1
                    (kl:>l <\ n, nt:>vt, kr:>r)
                )
            )
            -- reduce right
            (If (CmpSymbol n nt == GT)
                -- del right 6
                (kl:>l, nt:>vt, kr:>r <\ n)
                (If (CmpSymbol n nt == EQ)
                    -- del top 5
                    (kl:>l, TMax (kr:>r), kr:>r <\ TName (TMax (kr:>r)))
                    -- del left 4
                    (kl:>l <\n <+ nt:>vt, TMax (kr:>r), kr:>r <\ TName (TMax (kr:>r)))
                )
            )

-- internal classes ------
class NTAddB o a d b | o a d -> b where
    ntab :: Proxy o -> a -> d -> b

class NTDelB o a n b | o a n -> b where
    ntdb :: Proxy o -> a -> Proxy n -> b

------- instances -----
-- NTAdd: base
instance NTAdd (0:>()) (n:>v) (1:>(0:>(),n:>v,0:>())) where
    a <+ x = V (a, x, a)

-- NTAdd: for convenience
instance NTAdd () (n:>v) (1:>(0:>(),n:>v,0:>())) where
    () <+ x = V (V (), x, V ())

-- NTAdd: recursive
instance (k'~(k+1), NTAdd l nv l1) => NTAddB 1 (k:>(l,t,r)) nv (k':>(l1,t,r))
  where
    ntab _ (V (a,b,c)) d = V (a<+d,b,c)

instance (k'~(k+1), NTAdd r nv r1) => NTAddB 4 (k:>(l,t,r)) nv (k':>(l,t,r1))
  where
    ntab _ (V (a,b,c)) d = V (a,b,c<+d)

instance (k'~(k+1), NTAdd l t l1, NTAdd r nv r1
            , NTDel r1 mn r2, NTMin r1 (mn:>mv))
    => NTAddB 2 (k:>(l,t,r)) nv (k':>(l1,mn:>mv,r2))
  where
    ntab _ (V (a,b,c)) d = V (a<+b,mnv,r1<\mn)
      where
        r1 = c<+d
        mnv = getNTMin r1
        mn = Proxy :: Proxy mn

instance (k'~(k+1), NTAdd r t r1, NTAdd l nv l1
            , NTDel l1 mn l2, NTMax l1 (mn:>v))
    => NTAddB 3 (k:>(l,t,r)) nv (k':>(l2,mn:>v,r1))
  where
    ntab _ (V (a,b,c)) d = V (l1<\mn,mnv,c<+b)
      where
        l1 = a<+d
        mnv = getNTMax l1
        mn = Proxy :: Proxy mn

type CAdd kl kr n nt
    = If (CmpNat kl kr == GT)
        -- grow right
        (If (CmpSymbol n nt == GT) 4 {- add right -} 3 {- add left -})
        -- grow left
        (If (CmpSymbol n nt == GT) 2 {- add right -} 1 {- add left -})

instance (NTAddB (CAdd kl kr n nt) (k:>(kl:>l,nt:>vt,kr:>r)) (n:>v) c)
    => NTAdd (k:>(kl:>l,nt:>vt,kr:>r)) (n:>v) c
  where
    (<+) = ntab (Proxy :: Proxy (CAdd kl kr n nt))

-- NTDel: base
instance NTDel (1:>(0:>(),n:>v,0:>())) n (0:>()) where
    _ <\ _ = V ()

-- NTDel: recursive
instance (k'~(k-1), NTDel l n l1) => NTDelB 1 (k:>(l,t,r)) n (k':>(l1,t,r))
  where
    ntdb _ (V (a,b,c)) n = V (a<\n,b,c)

instance (k'~(k-1), NTDel r n r1) => NTDelB 6 (k:>(l,t,r)) n (k':>(l,t,r1))
  where
    ntdb _ (V (a,b,c)) n = V (a,b,c<\n)

instance (k'~(k-1), NTDel l mn l1, NTMax l (mn:>mv))
    => NTDelB 2 (k:>(l,t,r)) n (k':>(l1,mn:>mv,r))
  where
    ntdb _ (V (a,b,c)) _ = V (a<\mn,mnv,c)
      where
        mnv = getNTMax a
        mn = Proxy :: Proxy mn

instance (k'~(k-1), NTDel r mn r1, NTMin r (mn:>mv))
    => NTDelB 5 (k:>(l,t,r)) n (k':>(l,mn:>mv,r1))
  where
    ntdb _ (V (a,b,c)) _ = V (a,mnv,c<\mn)
      where
        mnv = getNTMin c
        mn = Proxy :: Proxy mn

instance (k'~(k-1), NTDel l mn l1, NTMax l (mn:>mv), NTDel r n r1, NTAdd r1 t r2)
    => NTDelB 3 (k:>(l,t,r)) n (k':>(l1,mn:>mv,r2))
  where
    ntdb _ (V (a,b,c)) n = V (a<\mn,mnv,c<\n<+b)
      where
        mnv = getNTMax a
        mn = Proxy :: Proxy mn

instance (k'~(k-1), NTDel r mn r1, NTMin r (mn:>mv), NTDel l n l1, NTAdd l1 t l2)
    => NTDelB 4 (k:>(l,t,r)) n (k':>(l2,mn:>mv,r1))
  where
    ntdb _ (V (a,b,c)) n = V (a<\n<+b,mnv,c<\mn)
      where
        mnv = getNTMin c
        mn = Proxy :: Proxy mn

type CDel kl kr n nt
    = If (CmpNat kr kl == LT)
        -- reduce left
        (If (CmpSymbol n nt == GT) 3 {- del right -}
            (If (CmpSymbol n nt == EQ) 2 {- del top -} 1 {- del left -})
        )
        -- reduce right
        (If (CmpSymbol n nt == GT) 6 {- del right -}
            (If (CmpSymbol n nt == EQ) 5 {- del top -} 4 {- del left -})
        )

instance (NTDelB (CDel kl kr n nt) (k:>(kl:>(ll,tl,rl),nt:>vt,kr:>r)) n c)
    => NTDel (k:>(kl:>(ll,tl,rl),nt:>vt,kr:>r)) n c
  where
    (<\) = ntdb (Proxy :: Proxy (CDel kl kr n nt))

-- NTMin, NTMax: base
instance NTMin (1:>(0:>(),b,0:>())) b where
    getNTMin (V (_,b,_)) = b

instance NTMax (1:>(0:>(),b,0:>())) b where
    getNTMax (V (_,b,_)) = b

instance NTMax (2:>(1:>a,b,0:>())) b where
    getNTMax (V (_,b,_)) = b

-- NTMin, NTMax: recursive
instance (NTMin (kl:>(ll,lt,lr)) ma) => NTMin (k:>(kl:>(ll,lt,lr),b,c)) ma where
    getNTMin (V (a,_,_)) = getNTMin a

instance (NTMax (kr:>(rl,rt,rr)) mc) => NTMax (k:>(a,b,kr:>(rl,rt,rr))) mc where
    getNTMax (V (_,_,c)) = getNTMax c

-- tests ---
type T1  = ()  <+ "1" :>Int
type T2  = T1  <+ "2" :>Int
type T3  = T2  <+ "3" :>Int
type T4  = T3  <+ "4" :>Int
type T5  = T4  <+ "5" :>Int
type T6  = T5  <+ "6" :>Int
type T7  = T6  <+ "7" :>Int
type T8  = T7  <+ "8" :>Int
type T9  = T8  <+ "9" :>Int
type T10 = T9  <+ "10":>Int
type T11 = T10 <+ "11":>Int
type T12 = T11 <+ "12":>Int
type T13 = T12 <+ "13":>Int
type T14 = T13 <+ "14":>Int
type T15 = T14 <+ "15":>Int
type T16 = T15 <+ "16":>Int
type T17 = T16 <+ "17":>Int
type T18 = T17 <+ "18":>Int
type T19 = T18 <+ "19":>Int
type T20 = T19 <+ "20":>Int
type T21 = T20 <+ "21":>Int
type T22 = T21 <+ "22":>Int
type T23 = T22 <+ "23":>Int
type T24 = T23 <+ "24":>Int
{-
test10  = ()    <+ (V 1 :: "1":>Int)
                <+ (V 2 :: "2":>Int)
                <+ (V 3 :: "3":>Int)
                <+ (V 4 :: "4":>Int)
                <+ (V 5 :: "5":>Int)
                <+ (V 6 :: "6":>Int)
                <+ (V 7 :: "7":>Int)
                <+ (V 8 :: "8":>Int)
                {-
                <+ (V 9 :: "9":>Int)
                <+ (V 10 :: "10":>Int)
                <+ (V 11 :: "11":>Int)
                <+ (V 12 :: "12":>Int)
                <+ (V 13 :: "13":>Int)
                -}
                -- <+ (V 14 :: "14":>Int)
       :: ()    <+ "1" :>Int
                <+ "2" :>Int
                <+ "3" :>Int
                <+ "4" :>Int
                <+ "5" :>Int
                <+ "6" :>Int
                <+ "7" :>Int
                <+ "8" :>Int
                {-
                <+ "9" :>Int
                <+ "10":>Int
                <+ "11":>Int
                <+ "12":>Int
                <+ "13":>Int
                -}
                -- <+ "14":>Int
-}

test1 = ()       <+ (V 1 :: "1":>Int)   :: T1
test2  = test1   <+ (V 2 :: "2":>Int)   :: T2
test3  = test2   <+ (V 3 :: "3":>Int)   :: T3
test4  = test3   <+ (V 4 :: "4":>Int)   :: T4
{-
test5  = test4   <+ (V 5 :: "5":>Int)   :: T5
test6  = test5   <+ (V 6 :: "6":>Int)   :: T6
test7  = test6   <+ (V 7 :: "7":>Int)   :: T7
test8  = test7   <+ (V 8 :: "8":>Int)   :: T8
test9  = test8   <+ (V 9 :: "9":>Int)   :: T9
test10 = test9   <+ (V 10 :: "10":>Int) :: T10
test11 = test10  <+ (V 11 :: "11":>Int) :: T11
test12 = test11  <+ (V 12 :: "12":>Int) :: T12
test13 = test12  <+ (V 13 :: "13":>Int) :: T13
test14 = test13  <+ (V 14 :: "14":>Int) :: T14
test15 = test14  <+ (V 15 :: "15":>Int) :: T15
test16 = test15  <+ (V 16 :: "16":>Int) :: T16
test17 = test16  <+ (V 17 :: "17":>Int) :: T17
test18 = test17  <+ (V 18 :: "18":>Int) :: T18
test19 = test18  <+ (V 19 :: "19":>Int) :: T19
test20 = test19  <+ (V 20 :: "20":>Int) :: T20
test21 = test20  <+ (V 21 :: "21":>Int) :: T21


f1  a = a <+ (V  1 ::  "1":>Int)
f2  a = a <+ (V  2 ::  "2":>Int)
f3  a = a <+ (V  3 ::  "3":>Int)
f4  a = a <+ (V  4 ::  "4":>Int)
f5  a = a <+ (V  5 ::  "5":>Int)
f6  a = a <+ (V  6 ::  "6":>Int)
f7  a = a <+ (V  7 ::  "7":>Int)
f8  a = a <+ (V  8 ::  "8":>Int)
f9  a = a <+ (V  9 ::  "9":>Int)
f10 a = a <+ (V 10 :: "10":>Int)
f11 a = a <+ (V 11 :: "11":>Int)
f12 a = a <+ (V 12 :: "12":>Int)
f13 a = a <+ (V 13 :: "13":>Int)
f14 a = a <+ (V 14 :: "14":>Int)
f15 a = a <+ (V 15 :: "15":>Int)
f16 a = a <+ (V 16 :: "16":>Int)
f17 a = a <+ (V 17 :: "17":>Int)
f18 a = a <+ (V 18 :: "18":>Int)
f19 a = a <+ (V 19 :: "19":>Int)
f20 a = a <+ (V 20 :: "20":>Int)
f21 a = a <+ (V 21 :: "21":>Int)


isTest10 = test10 ==
        V   ( V ( V ( V (V (),V 0,V ())
                    , V 1
                    , V ()
                    )
                , V 2
                , V ( V (V (), V 3, V ())
                    , V 4
                    , V ()
                    )
                )
            , V 5
            , V ( V ( V (V (), V 6, V ())
                    , V 7
                    , V ()
                    )
                , V 8
                , V (V (), V 9, V ())
                )
            )

isTestF = g1 () == g2 ()
  where
    g1 = f3.f5.f11.f13.f1.f2.f4.f6.f15
    g2= (f3.f5).f13.f11.f15.f6.f4.f1.f2
-}
