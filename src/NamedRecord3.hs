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
module NamedRecord3
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
type family TName a where
    TName ()        = ""
    TName (n:>v)    = n
proxyName :: s :> val -> Proxy s
proxyName (a :: s :> val) = Proxy :: Proxy s
proxyVal :: s :> val -> Proxy val
proxyVal (a :: s :> val) = Proxy :: Proxy val

data BTree a b c    = BTree { treeLeft :: !a, treeTop :: !b, treeRight :: !c }
    deriving (Eq, Show, Typeable)

proxyLeft  :: BTree a b c -> Proxy a
proxyLeft (BTree a b c) = Proxy :: Proxy a
proxyRight :: BTree a b c -> Proxy c
proxyRight (BTree a b c) = Proxy :: Proxy c

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
    type (<+) () ((n::Symbol) :> v) = BTree () (n:>v) ()
    type (<\) () (nt::Symbol) = ()
    type TLift () f = ()
    newRec _ = ()

instance CBTree ((nt::Symbol):>vt) where
    type Cnt (nt:>vt) = 1
    type TMin (nt:>vt) = nt:>vt
    type TMax (nt:>vt) = nt:>vt
    type (<+) (nt:>vt) ((n::Symbol) :> v)
        = If (CmpSymbol nt n == GT)
            (BTree (n:>v) (nt:>vt) ())
            (BTree (nt:>vt) (n:>v) ())
    type (<\) (nt:>vt) (nt::Symbol) = ()
    type TLift (nt:>vt) f = nt :> f vt
    newRec _ = V Nothing :: nt :> Maybe vt

{-
instance CBTree (BTree () ((nt::Symbol):>vt) ()) where
    type Cnt  (BTree () (nt:>vt) ()) = 1
    type TMin (BTree () (nt:>vt) ()) = nt:>vt
    type TMax (BTree () (nt:>vt) ()) = nt:>vt
    type (<+) (BTree () (nt:>vt) ()) ((n::Symbol) :> v)
        = -- grow left
            (If (CmpSymbol n nt == GT)
                -- add right 2
                ( BTree (nt:>vt) (n:>v) () )
                -- add left 1
                ( BTree (n:>v) (nt:>vt) () )
            )
    type  (<\) (BTree () (nt:>vt) ()) (nt::Symbol) = ()
    type TLift (BTree l (nt:>vt) r) f = BTree (TLift l f) (nt:>f vt) (TLift r f)
    newRec _ = BTree (newRec (Proxy :: Proxy l))
                    (V Nothing :: nt :> Maybe vt)
                    (newRec (Proxy :: Proxy r))
-}

type family CBAdd bt nv cn cs where
    CBAdd () ((n::Symbol) :> v) cn cs = BTree () (n:>v) ()
    CBAdd (nt:>vt) ((n::Symbol) :> v) cn GT = BTree (n:>v) (nt:>vt) ()
    CBAdd (nt:>vt) ((n::Symbol) :> v) cn cs = BTree (nt:>vt) (n:>v) ()
    CBAdd (BTree l (nt:>vt) r) ((n::Symbol) :> v) GT GT
        = BTree l (nt:>vt) (r <+ n:>v)
    CBAdd (BTree l (nt:>vt) r) ((n::Symbol) :> v) GT cs
        =   ( BTree ( l <+ n:>v <\ TName (TMax (l <+ n:>v)) )
                    ( TMax (l <+ n:>v) )
                    ( r <+ nt:>vt )
            )
    CBAdd (BTree l (nt:>vt) r) ((n::Symbol) :> v) cn GT
        =   ( BTree (l <+ nt:>vt)
                    (TMin (r <+ n:>v))
                    (r <+ n:>v <\ TName (TMin (r <+ n:>v)))
            )
    CBAdd (BTree l (nt:>vt) r) ((n::Symbol) :> v) cn cs
        = BTree (l <+ n:>v) (nt:>vt) r

type family CBDel bt n cn cs where
    -- CBDel () n cn cs = ()
    CBDel (nt:>vt) (nt::Symbol) cn cs = ()
    CBDel (BTree () (nt:>vt) ()) (nt::Symbol) cn cs = ()
    CBDel (BTree l (nt:>vt) r) (n::Symbol) LT GT
        = BTree ( l <\ TName (TMax l) )
                ( TMax l )
                ( r <\ n <+ nt:>vt )
    CBDel (BTree l (nt:>vt) r) (n::Symbol) LT EQ
        = BTree ( l <\ TName (TMax l) )
                ( TMax l )
                r
    CBDel (BTree l (nt:>vt) r) (n::Symbol) LT LT = BTree (l <\ n) (nt:>vt) r
    CBDel (BTree l (nt:>vt) r) (n::Symbol) cn GT = BTree l (nt:>vt) (r <\ n)
    CBDel (BTree l (nt:>vt) r) (n::Symbol) cn EQ
        = BTree l
                ( TMax r )
                ( r <\ TName (TMax r) )
    CBDel (BTree l (nt:>vt) r) (n::Symbol) cn LT
        = BTree ( l <\n <+ nt:>vt )
                ( TMax r )
                ( r <\ TName (TMax r) )

instance (CBTree l, CBTree r) => CBTree (BTree l ((nt::Symbol):>vt) r) where
    type Cnt  (BTree l (nt:>vt) r)
        = 1 + Cnt l + Cnt r
    type TMin (BTree l (nt:>vt) r)
        = If (l == ()) (nt:>vt) (TMin l)
    type TMax (BTree l (nt:>vt) r)
        = If (r == ()) (nt:>vt) (TMax r)
    type (<+) (BTree l (nt:>vt) r) ((n::Symbol) :> v)
        = CBAdd (BTree l (nt:>vt) r)
                ((n::Symbol) :> v)
                (CmpNat (Cnt l) (Cnt r))
                (CmpSymbol n nt)
    type  (<\) (BTree l (nt:>vt) r) (n::Symbol)
        = CBDel (BTree l (nt:>vt) r)
                (n::Symbol)
                (CmpNat (Cnt r) (Cnt l))
                (CmpSymbol n nt)
    type TLift (BTree l (nt:>vt) r) f = BTree (TLift l f) (nt:>f vt) (TLift r f)
    newRec _ = BTree (newRec (Proxy :: Proxy l))
                    (V Nothing :: nt :> Maybe vt)
                    (newRec (Proxy :: Proxy r))

{-
type family a <+ b where
    () <+ (n:>v) = BTree () (n:>v) ()
    BTree l (nt:>vt) r <+ (n:>v)
        = If (CmpNat (Cnt l) (Cnt r) == GT)
            -- grow right
            (If (CmpSymbol n nt == GT)
                -- add right 4
                BTree l (nt:>vt) (r <+ n:>v)
                -- add left 3
                ( l <+ n:>v <\ TName (TMax (l <+ n:>v))
                , TMax (l <+ n:>v)
                , r <+ nt:>vt
                )
            )
            -- grow left
            (If (CmpSymbol n nt == GT)
                -- add right 2
                ( l <+ nt:>vt
                , TMin (r <+ n:>v)
                , r <+ n:>v <\ TName (TMin (r <+ n:>v))
                )
                -- add left 1
                (l <+ n:>v, nt:>vt, r)
            )
type family (a :: *) <\ (b :: Symbol) :: * where
    ((),n:>v,()) <\ n = ()
    (l,nt:>vt,r) <\ n
        = If (CmpNat (Cnt r) (Cnt l) == LT)
            -- reduce left
            (If (CmpSymbol n nt == GT)
                -- del right 3
                ( l <\ TName (TMax l)
                , TMax l
                , r <\ n <+ nt:>vt
                )
                (If (CmpSymbol n nt == EQ)
                    -- del top 2
                    ( l <\ TName (TMax l)
                    , TMax l
                    , r
                    )
                    -- del left 1
                    (l <\ n, nt:>vt, r)
                )
            )
            -- reduce right
            (If (CmpSymbol n nt == GT)
                -- del right 6
                (l, nt:>vt, r <\ n)
                (If (CmpSymbol n nt == EQ)
                    -- del top 5
                    ( l
                    , TMax r
                    , r <\ TName (TMax r)
                    )
                    -- del left 4
                    ( l <\n <+ nt:>vt
                    , TMax r
                    , r <\ TName (TMax r)
                    )
                )
            )
-}
{-
type family LiftedRec f rec where
    LiftedRec f () = ()
    LiftedRec f (a,bn:>bv,c) = (LiftedRec f a, bn:>f bv, LiftedRec f c)

class CLiftedRec rec where
    zeroRec     :: (forall a. Proxy a -> f a) ->  LiftedRec f rec
    liftRec     :: (forall a. a -> f a) -> rec -> LiftedRec f rec
    unliftRec   :: (forall a. f a -> Either String a) -> LiftedRec f rec
                -> Either [String] rec

instance CLiftedRec () where
    zeroRec g = ()
    liftRec g () = ()
    unliftRec g () = Right ()

instance (CLiftedRec a, CLiftedRec c) => CLiftedRec (a,bn:>v,c) where
    zeroRec g         = (zeroRec g, V $ g (Proxy :: Proxy v), zeroRec g)
    liftRec g (a,b,c) = (liftRec g a, fmap g b, liftRec g c)
    unliftRec g (a, V mbv, c) = case (,,) <$> a1 <*> b1 <*> c1 of
        Left _  -> Left ls
        x -> x
      where
        bn = symbolVal (Proxy :: Proxy bn)
        a1 = unliftRec g a
        c1 = unliftRec g c
        b1 = g mbv
        ls = concat $ lefts [a1] ++ fmap (:[]) (lefts [b1]) ++ lefts [c1]

-- toRec :: (CLiftedRec r) => LiftedRec Maybe r -> Either [String] r
-- toRec = unliftRec (maybe (Left bn) (Right . V))
newRec :: (CLiftedRec r) => LiftedRec Maybe r
newRec = zeroRec (const Nothing)
-}

{-
class CNewRec a where
    type NewRec a
    newRec :: Proxy a -> NewRec a
    toRec :: NewRec a -> Either [String] a -- [String] - list of uninitialized fields
--    init  :: NewRec a -> M.Map String PersistentValue -> Either [String] a
-- we need to constraint field types like (Field v) => (n::Symbol):>v
-- for (Field v) we need convert :: PersistentValue -> v

instance CNewRec () where
    type NewRec () = ()
    newRec _ = ()
    toRec _ = Right ()

instance (CNewRec a, CNewRec c, KnownSymbol bn) => CNewRec (a, bn:>bv, c) where
    type NewRec (a,bn:>bv,c) =  ( NewRec a
                                , bn:>(Maybe bv)
                                , NewRec c
                                )
    newRec _ =  ( newRec (Proxy :: Proxy a)
                , V Nothing :: bn :> (Maybe bv)
                , newRec (Proxy :: Proxy c)
                )
    toRec (a, V mbv, c) = case (,,) <$> a1 <*> b1 <*> c1 of
        Left _  -> Left ls
        x -> x
      where
        bn = symbolVal (Proxy :: Proxy bn)
        a1 = toRec a
        c1 = toRec c
        b1 = maybe (Left [bn]) (Right . V) mbv
        ls = concat $ lefts [a1] ++ lefts [b1] ++ lefts [c1]
-}
{-
type family Person where
-- type
    Person = () <+ "name":>String
                <+ "age":>Int
                <+ "gender":>Bool
                <+ "year":>Int
                <+ "month":>Int
                <+ "day":>Int
                <+ "week":>Maybe Int
type family PersonMaybe where
    PersonMaybe = TLift Person Maybe

newPerson = newRec (Proxy :: Proxy Person)
-}
