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
module NamedRecord
    ( (:>)(..)
    , type (<+)
    -- , CNewRec(..)
    , NewRec
    , newRec
    , toRec
    , NamedRec
    , proxyNamed
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

class CNamedRec a where
    type NamedRec a

instance CNamedRec (0:>()) where
    type NamedRec (0:>()) = ()

instance (CNamedRec a, CNamedRec c) => CNamedRec ((k::Nat):>(a,b,c)) where
    type NamedRec ((k::Nat):>(a,b,c)) = (NamedRec a, b, NamedRec c)

proxyNamed :: a:>b -> Proxy a
proxyNamed a = Proxy :: Proxy a

type family TName a where
    TName (n:>v) = n

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


