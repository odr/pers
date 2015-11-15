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


class CList a where
    type (+>) b a
    type LiftedList a (f :: * -> *)-- (f :: *->*)
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
    ListTree0 ((na::Symbol):>va) ((nd::Symbol):>vd) ord
        = If (CmpSymbol na nd == GT)
            (((),nd:>vd,()), na:>va, ())
            ((), na:>va, ())
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
    -- type ListCnt ((n0::Symbol):>v0) = 1
    -- type ListTree ((n0::Symbol):>v0) = BTree () ((n0::Symbol):>v0) ()
    -- type ListThree ((n0::Symbol):>v0) = ((),(n0::Symbol):>v0, ())
    -- type ListPair ((n0::Symbol):>v0) = (((n0::Symbol):>v0),((n0::Symbol):>v0))

instance (CList a) => CList ((n0::Symbol):>v0, a) where
    type (+>) ((n::Symbol) :> v) ((n0::Symbol):>v0, a)
        = If (CmpSymbol n n0 == LT)
            (n:>v,(n0:>v0,a))
            (n0:>v0,n:>v +> a)
    type LiftedList ((n0::Symbol):>v0, a) f = (n0 :> f v0, LiftedList a f)
    newList _ = (V Nothing, newList (Proxy :: Proxy a))
    -- type ListCnt ((n0::Symbol):>v0, a) = 1 + ListCnt a
    -- type ListTree ((n0::Symbol):>v0, a) = BTree () ((n0::Symbol):>v0) ()
    -- type ListThree ((n0::Symbol):>v0, a) = ListSplit ((n0::Symbol):>v0, a) () (Middle (1 + Cnt a))


----------------------------------------

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

