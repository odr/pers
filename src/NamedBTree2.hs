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

-- neede for tests
{-# LANGUAGE FlexibleContexts #-}

module NamedBTree2
    ( NTMin(..)
    , (:>)(..)
    -- , (<\)
    , NTAdd(..)
    , NTDel(..)
    , NTMax(..)
    , TName
    -- , TMin
    -- , TMax
    ) where
import Fields ((:>)(..))
import Data.Typeable
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

-- infixr 9 :>
infixl 6 <+
infixl 6 <\

-- newtype s:>val = V val deriving (Typeable, Show, Eq, Ord)

-- interface: classes -----
class NTMin a where
    type TMin a
    getNTMin :: a -> TMin a

class NTMax a where
    type TMax a
    getNTMax :: a -> TMax a

type family TName a :: Symbol where
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

-- instances ---

-- NTMin, NTMax: base
instance NTMin (k:>(kl:>(),t,r)) where
    type TMin (k:>(kl:>(),t,r)) = t
    getNTMin (V (_,t,_)) = t

instance NTMax (k:>(l,t,kr:>())) where
    type TMax (k:>(l,t,kt:>())) = t
    getNTMax (V (_,t,_)) = t

{-
instance NTMin (0:>()) where
    type TMin (0:>()) = ()
    getNTMin (V ()) = ()

instance NTMax (0:>()) where
    type TMax (0:>()) = ()
    getNTMax (V ()) = ()
-}
-- NTMin, NTMax: recursive
instance (NTMin (kl:>(ll,lt,lr))) => NTMin (k:>(kl:>(ll,lt,lr),b,c)) where
    type TMin (k:>(kl:>(ll,lt,lr),b,c)) = TMin (kl:>(ll,lt,lr))
    getNTMin (V (l,_,_)) = getNTMin l

instance (NTMax (kr:>(rl,rt,rr))) => NTMax (k:>(l,t,kr:>(rl,rt,rr))) where
    type TMax (k:>(l,t,kr:>(rl,rt,rr))) = TMax (kr:>(rl,rt,rr))
    getNTMax (V (_,_,r)) = getNTMax r

class NTAdd a d where
    type a <+ d
    (<+) :: a -> d -> a <+ d

class NTDel a n where
    type a <\ n
    (<\) :: a -> Proxy n -> a <\ n

class NTAddB (c::Symbol) a d where
    type TAB c a d
    nab :: Proxy c -> a -> d -> TAB c a d

class NTDelB (c::Nat) a n where
    type TDB c a n
    ndb :: Proxy c -> a -> Proxy n -> TDB c a n

-- instance NTAdd: base
instance NTAdd () (n:>v) where
    type () <+ (n:>v) = (1:>(0:>(),n:>v,0:>()))
    () <+ v = V (V (), v, V ())

instance NTAdd (0:>()) (n:>v) where
    type (0:>()) <+ (n:>v) = (1:>(0:>(),n:>v,0:>()))
    (V ()) <+ v = V (V (), v, V ())

--instance NtAddB and NTAdd: recursive
type CAdd kl l kr r n nt
    = If (CmpNat kl kr == GT)
        -- grow right
        (If (CmpSymbol n nt == GT)
            -- add right
            "gr-ar"
            -- add left
            (If (kl == 0 || CmpSymbol (TName (TMax (kl:>l))) n == LT)
                "gr-al-top"
                (If (IsEven kl) "gr-al-even" "gr-al-odd")
            )
        )
        -- grow left
        (If (CmpSymbol n nt == GT)
            -- add right
            (If (kr == 0 || CmpSymbol (TName (TMin (kr:>r))) n == GT)
                "gl-ar-top"
                (If (IsEven kr) "gl-ar-even" "gl-ar-odd")
            )
            -- add left
            "gl-al"
        )

instance (NTAddB (CAdd kl l kr r n nt) (k:>(kl:>l,nt:>vt,kr:>r)) (n:>v))
    => NTAdd (k:>(kl:>l,nt:>vt,kr:>r)) (n:>v)
  where
    type (k:>(kl:>l,nt:>vt,kr:>r)) <+ (n:>v)
        = TAB (CAdd kl l kr r n nt) (k:>(kl:>l,nt:>vt,kr:>r)) (n:>v)
    (<+) = nab (Proxy :: Proxy (CAdd kl l kr r n nt))

instance (NTAdd l d) => NTAddB "gl-al" ((k::Nat):>(l,t,r)) d where
    type TAB "gl-al" (k:>(l,t,r)) d = (k+1):>(l<+d,t,r)
    nab _ (V (l,t,r)) d  = V (l<+d,t,r)

instance (NTAdd r d) => NTAddB "gr-ar" ((k::Nat):>(l,t,r)) d where
    type TAB "gr-ar" (k:>(l,t,r)) d = (k+1):>(l,t,r<+d)
    nab _ (V (l,t,r)) d  = V (l,t,r<+d)

instance (NTAdd (kl:>l) (nt:>vt))
    => NTAddB "gl-ar-top" ((k::Nat):>((kl):>l,nt:>vt,(kr):>r)) d
  where
    type TAB "gl-ar-top" (k:>(kl:>l,nt:>vt,kr:>r)) d
        = (k+1) :>  ( kl:>l <+ nt:>vt
                    , d
                    , kr:>r
                    )
    nab _ (V (l,t,r)) d  = V (l<+t,d,r)

instance (NTAdd (kl:>l) (nt:>vt), NTMin (kr:>r)
        , NTAdd (kr:>r) d
        , NTDel (kr:>r <+ d) (TName (TMin (kr:>r)))
        )
    => NTAddB "gl-ar-even" ((k::Nat):>((kl::Nat):>l,nt:>vt,(kr::Nat):>r)) d
  where
    type TAB "gl-ar-even" (k:>(kl:>l,nt:>vt,kr:>r)) d
        = (k+1) :>  ( kl:>l <+ nt:>vt
                    , TMin (kr:>r)
                    , kr:>r <+ d <\ TName (TMin (kr :> r)) -- no problem!
                    )
    nab _ (V (l,t,r)) d  = V (l<+t, getNTMin r, r <+ d <\mn )
      where
        mn = Proxy :: Proxy (TName (TMin (kr :> r)))

instance (NTAdd (kl:>l) (nt:>vt), NTMin (kr:>r)
        , NTDel (kr:>r) (TName (TMin (kr :> r)))
        , NTAdd (kr:>r <\ TName (TMin (kr :> r))) d
        )
    => NTAddB "gl-ar-odd" ((k::Nat):>((kl::Nat):>l,nt:>vt,(kr::Nat):>r)) d
  where
    type TAB "gl-ar-odd" (k:>(kl:>l,nt:>vt,kr:>r)) d
        = (k+1) :>  ( kl:>l <+ nt:>vt
                    , TMin (kr:>r)
                    , kr:>r <\ TName (TMin (kr:>r)) <+ d  -- ok!
                    )
    nab _ (V (l,t,r)) d  = V (l<+t, getNTMin r, r <\ mn <+ d)
      where
        mn = Proxy :: Proxy (TName (TMin (kr :> r)))

instance (NTAdd (kr:>r) (nt:>vt))
    => NTAddB "gr-al-top" ((k::Nat):>((kl):>l,nt:>vt,(kr):>r)) d
  where
    type TAB "gr-al-top" (k:>(kl:>l,nt:>vt,kr:>r)) d
        = (k+1) :>  ( kl:>l
                    , d
                    , kr:>r <+ nt:>vt
                    )
    nab _ (V (l,t,r)) d  = V (l,d,r<+t)

instance (NTAdd (kr:>r) (nt:>vt), NTMax (kl:>l)
        , NTDel (kl:>l) (TName (TMax (kl:>l)))
        , NTAdd (kl:>l <\ TName (TMax (kl :> l))) d)
    => NTAddB "gr-al-even" ((k::Nat):>((kl::Nat):>l,nt:>vt,(kr::Nat):>r)) d
  where
    type TAB "gr-al-even" (k:>(kl:>l,nt:>vt,kr:>r)) d
        = (k+1) :>  ( kl:>l <\ TName (TMax (kl:>l)) <+ d -- ok!
                    , TMax (kl:>l)
                    , kr:>r <+ nt:>vt
                    )
    nab _ (V (l,t,r)) d  = V (l <\ mn <+ d, getNTMax l, r <+ t )
      where
        mn = Proxy :: Proxy (TName (TMax (kl :> l)))

instance (NTAdd (kr:>r) (nt:>vt), NTMax (kl:>l)
        , NTAdd (kl:>l) d
        , NTDel (kl:>l<+d) (TName (TMax (kl:>l)))
        )
    => NTAddB "gr-al-odd" ((k::Nat):>((kl::Nat):>l,nt:>vt,(kr::Nat):>r)) d
  where
    type TAB "gr-al-odd" (k:>(kl:>l,nt:>vt,kr:>r)) d
        = (k+1) :>  ( kl:>l <+ d <\ TName (TMax (kl:>l)) -- no problem!
                    , TMax (kl:>l)
                    , kr:>r <+ nt:>vt
                    )
    nab _ (V (l,t,r)) d  = V (l <+ d <\ mn, getNTMax l, r <+ t )
      where
        mn = Proxy :: Proxy (TName (TMax (kl :> l)))

-- instance NTDel: base
instance NTDel (1:>(0:>(),n:>v,0:>())) n where
    type (1:>(0:>(),n:>v,0:>())) <\ n = 0:>()
    _ <\ _ = V ()

-- instance NTDelB, NTDel: recursive
type CDel kl l kr r n nt
    = If (CmpNat kr kl == LT)
        -- reduce left
        (If (CmpSymbol n nt == GT) 3 {- del right -}
            (If (CmpSymbol n nt == EQ) 2 {- del top -} 1 {- del left -})
        )
        -- reduce right
        (If (CmpSymbol n nt == GT) 6 {- del right -}
            (If (CmpSymbol n nt == EQ) 5 {- del top -} 4 {- del left -})
        )

instance (NTDelB (CDel kl (ll,lt,lr) kr r n nt) (k:>(kl:>(ll,lt,lr),nt:>vt,kr:>r)) n)
    => NTDel (k:>(kl:>(ll,lt,lr),nt:>vt,kr:>r)) n
  where
    type (k:>(kl:>(ll,lt,lr),nt:>vt,kr:>r)) <\ n
        = TDB (CDel kl (ll,lt,lr) kr r n nt) (k:>(kl:>(ll,lt,lr),nt:>vt,kr:>r)) n
    (<\) = ndb (Proxy :: Proxy (CDel kl (ll,lt,lr) kr r n nt))

instance (NTDel l n) => NTDelB 1 ((k::Nat):>(l,t,r)) n where
    type TDB 1 ((k::Nat):>(l,t,r)) n = (k-1):>(l<\n,t,r)
    ndb _ (V (l,t,r)) n = V (l<\n,t,r)

instance (NTDel r n) => NTDelB 6 ((k::Nat):>(l,t,r)) n where
    type TDB 6 ((k::Nat):>(l,t,r)) n = (k-1):>(l,t,r<\n)
    ndb _ (V (l,t,r)) n = V (l,t,r<\n)

instance (NTDel l (TName (TMax l)), NTMax l) => NTDelB 2 ((k::Nat):>(l,t,r)) n where
    type TDB 2 ((k::Nat):>(l,t,r)) n = (k-1):>(l<\TName (TMax l),TMax l,r)
    ndb _ (V (l,t,r)) _ = V (l<\mn,mnv,r)
      where
        mnv = getNTMax l
        mn = Proxy :: Proxy (TName (TMax l))

instance (NTDel r (TName (TMin r)), NTMin r) => NTDelB 5 ((k::Nat):>(l,t,r)) n where
    type TDB 5 ((k::Nat):>(l,t,r)) n = (k-1):>(l,TMin r,r<\TName (TMin r))
    ndb _ (V (l,t,r)) _ = V (l,mnv,r<\mn)
      where
        mnv = getNTMin r
        mn = Proxy :: Proxy (TName (TMin r))

instance (NTDel l (TName (TMax l)), NTMax l, NTDel r n, NTAdd (r<\n) t)
    => NTDelB 3 ((k::Nat):>(l,t,r)) n
  where
    type TDB 3 ((k::Nat):>(l,t,r)) n = (k-1):>(l<\TName (TMax l),TMax l,r<\n<+t)
    ndb _ (V (l,t,r)) n = V (l<\mn,mnv,r<\n<+t)
      where
        mnv = getNTMax l
        mn = Proxy :: Proxy (TName (TMax l))

instance (NTDel r (TName (TMin r)), NTMin r, NTDel l n, NTAdd (l<\n) t)
    => NTDelB 4 ((k::Nat):>(l,t,r)) n
  where
    type TDB 4 ((k::Nat):>(l,t,r)) n = (k-1):>(l<\n<+t,TMin r,r<\TName (TMin r))
    ndb _ (V (l,t,r)) n = V (l<\n<+t,mnv,r<\mn)
      where
        mnv = getNTMin r
        mn = Proxy :: Proxy (TName (TMin r))

{-
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
-}
{-
test10  = ()    <+ (V 1 :: "1":>Int)
                <+ (V 2 :: "2":>Int)
                <+ (V 3 :: "3":>Int)
                <+ (V 4 :: "4":>Int)
                <+ (V 5 :: "5":>Int)
                <+ (V 6 :: "6":>Int)
                <+ (V 7 :: "7":>Int)
                <+ (V 8 :: "8":>Int)
                <+ (V 9 :: "9":>Int)
                <+ (V 10 :: "10":>Int)
                {-
                <+ (V 11 :: "11":>Int)
                <+ (V 12 :: "12":>Int)
                <+ (V 13 :: "13":>Int)
                -}
                -- <+ (V 14 :: "14":>Int)
       -- :: T8
                {- ()
                <+ "1" :>Int
                <+ "2" :>Int
                <+ "3" :>Int
                <+ "4" :>Int
                <+ "5" :>Int
                <+ "6" :>Int
                <+ "7" :>Int
                <+ "8" :>Int
                -}
                {-
                <+ "9" :>Int
                <+ "10":>Int
                <+ "11":>Int
                <+ "12":>Int
                <+ "13":>Int
                -}
                -- <+ "14":>Int
-}
{-

test1 = ()       <+ (V 1 :: "1":>Int)   :: T1
test2  = test1   <+ (V 2 :: "2":>Int)   :: T2
test3  = test2   <+ (V 3 :: "3":>Int)   :: T3
test4  = test3   <+ (V 4 :: "4":>Int)   :: T4
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
test22 = test21  <+ (V 22 :: "22":>Int) :: T22

-}

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

f1_2 a = f2.f1 $ a
f1_10 a = f10.f9.f8.f7.f6.f5.f4.f3.f2.f1 $ a
f1_20 a = f20.f19.f18.f17.f16.f15.f14.f13.f12.f11.f1_10 $ a
f1_20' a = (f20.f19.f18.f17.f16.f15.f14.f13.f12.f11).f1_10 $ a
{-
-}
{-
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
-}
