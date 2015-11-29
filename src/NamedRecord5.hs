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
module NamedRecord5
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
infixl 6 <+
infixl 6 <\

newtype s :> val = V val deriving (Typeable, Show, Eq, Ord, Functor)
class Named a where
    type TName a :: Symbol
instance Named ((n::Symbol):>v) where
    type TName (n:>v) = n
{-
type family TName a where
    TName ()        = ""
    TName (n:>v)    = n
-}
proxyName :: s :> val -> Proxy s
proxyName (a :: s :> val) = Proxy :: Proxy s
proxyVal :: s :> val -> Proxy val
proxyVal (a :: s :> val) = Proxy :: Proxy val

{-
data BTree a b c    = BTree { treeLeft :: !a, treeTop :: !b, treeRight :: !c }
    deriving (Eq, Show, Typeable)

proxyLeft  :: BTree a b c -> Proxy a
proxyLeft (BTree a b c) = Proxy :: Proxy a
proxyRight :: BTree a b c -> Proxy c
proxyRight (BTree a b c) = Proxy :: Proxy c
-}
class CBTree a where
    type Cnt a :: Nat
    type TMin a
    type TMax a
    type  (<+) a b
    type  (<\) a (b::Symbol)
    type TLift a (f :: * -> *)
    newRec :: Proxy a -> TLift a Maybe

instance CBTree () where
    type Cnt () = 0
    type TMin () = ()
    type TMax () = ()
    type (<+) () ((n::Symbol) :> v) = n:>v
    type (<\) () (nt::Symbol) = ()
    type TLift () f = ()
    newRec _ = ()

instance CBTree ((nt::Symbol):>vt) where
    type Cnt (nt:>vt) = 1
    type TMin (nt:>vt) = nt:>vt
    type TMax (nt:>vt) = nt:>vt
    type (<+) (nt:>vt) ((n::Symbol) :> v)
        = If (CmpSymbol nt n == GT) (n:>v, nt:>vt) (nt:>vt, n:>v)
    type (<\) (nt:>vt) (nt::Symbol) = ()
    type TLift (nt:>vt) f = nt :> f vt
    newRec _ = V Nothing :: nt :> Maybe vt

instance CBTree ((n1::Symbol):>v1, (n2::Symbol):>v2) where
    type Cnt (n1:>v1,n2:>v2) = 2
    type TMin (n1:>v1,n2:>v2) = n1:>v1
    type TMax (n1:>v1,n2:>v2) = n2:>v2
    type (<+) (n1:>v1,n2:>v2) ((n::Symbol) :> v)
        = If (CmpSymbol n1 n == GT) (n:>v, n1:>v1,n2:>v2)
            (If (CmpSymbol n n2 == GT) (n1:>v1, n2:>v2, n:>v)
                (n1:>v1, n:>v, n2:>v2)
            )
    type (<\) (n1:>v1,n2:>v2) (n::Symbol) =
        If (CmpSymbol n n1 == EQ) (n2:>v2) (n1:>v1)
    type TLift (n1:>v1, n2:>v2) f = (n1 :> f v1, n2 :> f v2)
    newRec _ = (V Nothing :: n1 :> Maybe v1, V Nothing :: n2 :> Maybe v2)
{-
instance CBTree ((n1::Symbol):>v1, (n2::Symbol):>v2, (n3::Symbol):>v3) where
    type Cnt (n1:>v1,n2:>v2,n3:>v3) = 3
    type TMin (n1:>v1,n2:>v2,n3:>v3) = n1:>v1
    type TMax (n1:>v1,n2:>v2,n3:>v3) = n3:>v3
    type (<+) (n1:>v1,n2:>v2,n3:>v3) ((n::Symbol) :> v)
        = If (CmpSymbol n1 n == GT) ((n:>v, n1:>v1),n2:>v2,n3:>v3)
            (If (CmpSymbol n n3 == GT) ((n1:>v1, n2:>v2), n3:>v3, n:>v)
                (If (CmpSymbol n2 n == GT) ((n1:>v1,n:>v),n2:>v2, n3:>v3)
                    ((n1:>v1, n2:>v2), n:>v, n3:>v3)
                )
            )
    type (<\) (n1:>v1,n2:>v2,n3:>v3) (n::Symbol) =
        If (CmpSymbol n n1 == EQ) (n2:>v2,n3:>v3)
            (If (CmpSymbol n n2 == EQ) (n1:>v1,n3:>v3) (n1:>v1,n2:>v2))
    type TLift (n1:>v1, n2:>v2, n3:>v3) f = (n1 :> f v1, n2 :> f v2, n3 :> f v3)
    newRec _ = (V Nothing :: n1 :> Maybe v1, V Nothing :: n2 :> Maybe v2, V Nothing :: n3 :> Maybe v3)
-}

type family CBAdd bt nv cn cs where
    CBAdd () ((n::Symbol) :> v) cn cs = n:>v
    CBAdd (nt:>vt) ((n::Symbol) :> v) cn GT = (n:>v, nt:>vt)
    CBAdd (nt:>vt) ((n::Symbol) :> v) cn cs = (nt:>vt,n:>v)
    CBAdd (n1:>v1, n2:>v2) ((n::Symbol) :> v) cn GT
        = (n:>v, n1:>v1,n2:>v2)
    CBAdd (n1:>v1, n2:>v2) ((n::Symbol) :> v) cn cs
        = If (CmpSymbol n n2 == GT) (n1:>v1, n2:>v2, n:>v)
                (n1:>v1, n:>v, n2:>v2)
    -- recursive
    CBAdd (l, nt:>vt, r) ((n::Symbol) :> v) GT GT
        = (l, nt:>vt, r <+ n:>v)
    CBAdd (l, nt:>vt, r) ((n::Symbol) :> v) GT cs
        =   ( l <+ n:>v <\ TName (TMax (l <+ n:>v))
            , TMax (l <+ n:>v)
            , r <+ nt:>vt
            )
    CBAdd (l, nt:>vt, r) ((n::Symbol) :> v) cn GT
        =   ( l <+ nt:>vt
            , TMin (r <+ n:>v)
            , r <+ n:>v <\ TName (TMin (r <+ n:>v))
            )
    CBAdd (l, nt:>vt, r) ((n::Symbol) :> v) cn cs
        = (l <+ n:>v, nt:>vt, r)

type family CBDel bt n cn cs where
    CBDel (nt:>vt) (nt::Symbol) cn cs = ()
    CBDel (n1:>v1,n2:>v2) (n::Symbol) cn cs =
        If (CmpSymbol n n1 == EQ) (n2:>v2) (n1:>v1)
    CBDel (n1:>v1,n2:>v2,n3:>v3) (n::Symbol) cn cs =
        If (CmpSymbol n n1 == EQ) (n2:>v2,n3:>v3)
            (If (CmpSymbol n n2 == EQ) (n1:>v1,n3:>v3)
                (n1:>v1,n2:>v2)
            )
    -- recursive
    CBDel (l, nt:>vt, r) (n::Symbol) LT GT
        =   ( l <\ TName (TMax l)
            , TMax l
            , r <\ n <+ nt:>vt
            )
    CBDel (l, nt:>vt, r) (n::Symbol) LT EQ
        =   ( l <\ TName (TMax l)
            , TMax l
            , r
            )
    CBDel (l, nt:>vt, r) (n::Symbol) LT LT = (l <\ n, nt:>vt, r)
    CBDel (l, nt:>vt, r) (n::Symbol) cn GT = (l, nt:>vt, r <\ n)
    CBDel (l, nt:>vt, r) (n::Symbol) cn EQ
        =   ( l
            , TMax r
            , r <\ TName (TMax r)
            )
    CBDel (l, nt:>vt, r) (n::Symbol) cn LT
        =   ( l <\n <+ nt:>vt
            , TMax r
            , r <\ TName (TMax r)
            )

instance (CBTree l, CBTree r) => CBTree (l, (nt::Symbol):>vt, r) where
    type Cnt  (l, nt:>vt, r) = 1 + Cnt l + Cnt r
    type TMin (l, nt:>vt, r) = TMin l
    type TMax (l, nt:>vt, r) = TMax r
    type (<+) (l, nt:>vt, r) ((n::Symbol) :> v)
        = CBAdd (l, nt:>vt, r)
                ((n::Symbol) :> v)
                (CmpNat (Cnt l) (Cnt r))
                (CmpSymbol n nt)
    type  (<\) (l, nt:>vt, r) (n::Symbol)
        = CBDel (l, nt:>vt, r)
                (n::Symbol)
                (CmpNat (Cnt r) (Cnt l))
                (CmpSymbol n nt)
    type TLift (l, nt:>vt, r) f = (TLift l f, nt:>f vt, TLift r f)
    newRec _ =  ( newRec (Proxy :: Proxy l)
                , V Nothing :: nt :> Maybe vt
                , newRec (Proxy :: Proxy r)
                )

