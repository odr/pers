{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module NamedRecord4
   {-
   ( (:>)(..)
    , type (<+)
    -- , CNewRec(..)
    , NewRec
    , newRec
    , toRec
    )
    -} where

import Data.Typeable(Proxy(..), Typeable(..))
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Either(either, lefts)

infixr 9 :>
-- infixl 6 <+
-- infixl 6 <\
infixr 6 +>

newtype s :> val = V val deriving (Typeable, Show, Eq, Ord, Functor)
type family TName a where
    TName ()        = ""
    TName (n:>v)    = n
proxyName :: s :> val -> Proxy s
proxyName (a :: s :> val) = Proxy :: Proxy s
proxyVal :: s :> val -> Proxy val
proxyVal (a :: s :> val) = Proxy :: Proxy val

proxyLeft  :: (a, b, c) -> Proxy a
proxyLeft (a, b, c) = Proxy :: Proxy a
proxyRight :: (a, b, c) -> Proxy c
proxyRight (a, b, c) = Proxy :: Proxy c


class CList a where
    type (+>) b a
    type LiftedList a (f :: * -> *)
    newList :: Proxy a -> LiftedList a Maybe

type family ListRev0 a b where
    ListRev0 ((n0::Symbol):>v0) () = n0:>v0
    ListRev0 ((n0::Symbol):>v0) b = ((n0:>v0), b)
    ListRev0 ((n0::Symbol):>v0, a) () = ListRev0 a (n0:>v0)
    ListRev0 ((n0::Symbol):>v0, a) b = ListRev0 a (n0:>v0, b)

type ListRev a = ListRev0 a ()
type ListTree a = ListTree1 a (ListRev a)

type family CmpHead a b where
    CmpHead () () = EQ
    CmpHead ((na::Symbol):>va) ((nc::Symbol):>vc) = CmpSymbol na nc
    CmpHead ((na::Symbol):>va,b) ((nc::Symbol):>vc,d) = CmpSymbol na nc

type family ListTree1 asc desc where
    ListTree1 a b = ListTree0 a b (CmpHead a b)

type family ListTree0 asc desc ord where
    ListTree0 () () b = ()
    ListTree0 ((na::Symbol):>va) ((nd::Symbol):>vd) GT = (nd:>vd, na:>va)
    ListTree0 ((na::Symbol):>va) ((nd::Symbol):>vd) EQ = na:>va
    ListTree0 ((na::Symbol):>va, (na2::Symbol):>va2) ((nd::Symbol):>vd,(nd2::Symbol):>vd2) GT
        = (nd:>vd, na:>va, na2:>va2)
    ListTree0 ((na::Symbol):>va, (na2::Symbol):>va2) ((nd::Symbol):>vd,(nd2::Symbol):>vd2) EQ
        = (na:>va, na2:>va2)
    ListTree0   ((na::Symbol):>va, ((na2::Symbol):>va2, (na3::Symbol):>va3))
                ((nd::Symbol):>vd, ((nd2::Symbol):>vd2, (nd3::Symbol):>vd3))
                GT
        = ((nd:>vd, na:>va), na2:>va2, na3:>va3)
    ListTree0   ((na::Symbol):>va, ((na2::Symbol):>va2, (na3::Symbol):>va3))
                ((nd::Symbol):>vd, ((nd2::Symbol):>vd2, (nd3::Symbol):>vd3))
                EQ
        = (na:>va, na2:>va2, na3:>va3)
    ListTree0   ((na::Symbol):>va, ((na2::Symbol):>va2, ((na3::Symbol):>va3, (na4::Symbol):>va4)))
                ((nd::Symbol):>vd, ((nd2::Symbol):>vd2, ((nd3::Symbol):>vd3, (nd4::Symbol):>vd4)))
                GT
        = ((nd:>vd, na:>va), na2:>va2, (na3:>va3, na4:>va4))
    ListTree0   ((na::Symbol):>va, ((na2::Symbol):>va2, ((na3::Symbol):>va3, (na4::Symbol):>va4)))
                ((nd::Symbol):>vd, ((nd2::Symbol):>vd2, ((nd3::Symbol):>vd3, (nd4::Symbol):>vd4)))
                EQ
        = ((na:>va, na2:>va2), na3:>va3, na4:>va4)
    {-
    ListTree0   ((na::Symbol):>va, ((na2::Symbol):>va2, ((na3::Symbol):>va3
                    , ((na4::Symbol):>va4, (na5::Symbol):>va5))))
                ((nd::Symbol):>vd, ((nd2::Symbol):>vd2, ((nd3::Symbol):>vd3
                    , ((nd4::Symbol):>vd4, (nd5::Symbol):>vd5))))
                GT
        = ((nd:>vd, na:>va, na2:>va2), na3:>va3, (na4:>va4, na5:>va5))
    ListTree0   ((na::Symbol):>va, ((na2::Symbol):>va2, ((na3::Symbol):>va3
                    , ((na4::Symbol):>va4, (na5::Symbol):>va5))))
                ((nd::Symbol):>vd, ((nd2::Symbol):>vd2, ((nd3::Symbol):>vd3
                    , ((nd4::Symbol):>vd4, (nd5::Symbol):>vd5))))
                EQ
        = ((na:>va, na2:>va2), na3:>va3, (na4:>va4, na5:>va5))
    -}
    -- recursive
    ListTree0 ((na::Symbol):>va, a) ((nd::Symbol):>vd, d) LT = ListTree1 a d

    ListTree0 ((na::Symbol):>va, a) ((nd::Symbol):>vd,d) EQ
        = ( ListTree1 (ListRev d) d, na:>va, ListTree1 a (ListRev a) )

    ListTree0 ((na::Symbol):>va, a) ((nd::Symbol):>vd, d) GT
        =   ( ListTree1 (ListRev (nd:>vd,d)) (nd:>vd,d)
            , na:>va
            , ListTree1 a (ListRev a)
            )

instance CList ((n0::Symbol):>v0) where
    type (+>) ((n::Symbol) :> v) ((n0::Symbol):>v0)
        = If (CmpSymbol n n0 == LT) (n:>v,n0:>v0) (n0:>v0,n:>v)
    type LiftedList ((n0::Symbol):>v0) f = n0 :> f v0
    newList _ = V Nothing

instance (CList a) => CList ((n0::Symbol):>v0, a) where
    type (+>) ((n::Symbol) :> v) ((n0::Symbol):>v0, a)
        = If (CmpSymbol n n0 == LT)
            (n:>v,(n0:>v0,a))
            (n0:>v0,n:>v +> a)
    type LiftedList ((n0::Symbol):>v0, a) f = (n0 :> f v0, LiftedList a f)
    newList _ = (V Nothing, newList (Proxy :: Proxy a))


class CRec a where
    type LiftedRec a (f :: * -> *)
    newRec :: Proxy a -> LiftedRec a Maybe

instance CRec ((n::Symbol):>v) where
    type LiftedRec (n:>v) f = n :> f v
    newRec _ = V Nothing

instance CRec ((n::Symbol):>v, (n2::Symbol):>v2) where
    type LiftedRec (n:>v,n2:>v2) f = (n :> f v, n2 :> f v2)
    newRec _ = (V Nothing, V Nothing)

instance CRec () where
    type LiftedRec () f = ()
    newRec _ = ()

instance (CRec l, CRec r) => CRec (l, (n::Symbol):>v, r) where
    type LiftedRec (l, (n::Symbol):>v, r) f
        = (LiftedRec l f, n :> f v, LiftedRec r f)
    newRec _ = (newRec (Proxy :: Proxy l), V Nothing, newRec (Proxy :: Proxy r))

----------------------------------------
{-
type family PersonList where
-- type
    PersonList = "name":>String
                +> "age":>Int
                +> "gender":>Bool
                +> "year":>Int
                +> "month":>Int
                +> "day":>Int
                +> "week":>Maybe Int

{-
type family PersonList1 where
    PersonList1 = ListThree PersonList
-}
type family Nums where
    Nums    = "1":>Int
           +> "2":>Int
           +> "3":>Int
           +> "4":>Int
           +> "5":>Int
           +> "6":>Int
           +> "7":>Int
           +> "8":>Int
           +> "9":>Int
           +> "A":>Int
           +> "B":>Int
           +> "C":>Int
           +> "D":>Int
           +> "E":>Int
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
{-
-}
plNums = Proxy :: Proxy Nums
plNumsRev = Proxy :: Proxy (ListRev Nums)
-- plNumsPair = Proxy :: Proxy (ListPair Nums)
plNumsTree = Proxy :: Proxy (ListTree Nums)
-- plNums1 = Proxy :: Proxy (ListRL Nums () True)

-}
