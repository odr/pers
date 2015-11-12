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
module NamedRecord2
    ( (:>)(..)
    , type (<+)
    -- , CNewRec(..)
    , NewRec
    , newRec
    , toRec
    ) where

import Data.Typeable(Proxy(..), Typeable(..))
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Either(either, lefts)

infixr 9 :>
infixl 6 <+
infixl 6 <\

newtype s :> val = V val deriving (Typeable, Show, Eq, Ord)

type family TName a where
    TName (n:>v) = n

type family Cnt a :: Nat where
    Cnt () = 0
    Cnt (a,b,c) = 1 + Cnt a + Cnt c

type family TMin a where
    TMin ((),b,c)   = b
    TMin (a,b,c)    = TMin a

type family TMax a where
    TMax (a,b,())   = b
    TMax (a,b,c)    = TMax c

type family a <+ b where
    () <+ (n:>v) = ((),n:>v,())
    (l,nt:>vt,r) <+ (n:>v)
        = If (CmpNat (Cnt l) (Cnt r) == GT)
            -- grow right
            (If (CmpSymbol n nt == GT)
                -- add right 4
                (l,nt:>vt,r <+ n:>v)
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
-}
