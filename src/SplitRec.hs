{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SplitRec where

import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Tuple
import Control.Lens.Getter
import Control.Lens.Setter

-- newtype (s :: Symbol) :> val = V val deriving (Typeable, Show)
-- infixr 9 :>

data Rec1 n1 v1 = Rec1 v1                           -- v1 -> Rec1 n1 v1
data Rec2 n1 v1 n2 v2 = Rec2 v1 v2                  -- v1 -> v2 -> Rec2 n1 v1 n2 v2
data Rec3 n1 v1 n2 v2 n3 v3 = Rec3 v1 v2 v3
data Rec4 n1 v1 n2 v2 n3 v3 n4 v4 = Rec4 v1 v2 v3 v4
data Rec5 n1 v1 n2 v2 n3 v3 n4 v4 n5 v5 = Rec5 v1 v2 v3 v4 v5
data Rec6 n1 v1 n2 v2 n3 v3 n4 v4 n5 v5 n6 v6 = Rec6 v1 v2 v3 v4 v5 v6
data Rec7 n1 v1 n2 v2 n3 v3 n4 v4 n5 v5 n6 v6 n7 v7 = Rec7 v1 v2 v3 v4 v5 v6 v7
data Rec8 n1 v1 n2 v2 n3 v3 n4 v4 n5 v5 n6 v6 n7 v7 n8 v8 = Rec8 v1 v2 v3 v4 v5 v6 v7 v8
data Rec9 n1 v1 n2 v2 n3 v3 n4 v4 n5 v5 n6 v6 n7 v7 n8 v8 n9 v9 = Rec9 v1 v2 v3 v4 v5 v6 v7 v8 v9

{-
class ParamCnt (n::Nat) a where
instance ParamCnt 0 a where
instance (ParamCnt k b, (k+1) ~ n) => ParamCnt n (a->b) where
-}

class SplitIso a a' b c b' c' | a -> b c, b c -> a, b' c' -> a', a' -> b' c'
  where
    splitIso :: Iso a a' (b,c) (b',c')

instance SplitIso (Rec2 n1 v1 n2 v2) (Rec2 n3 v3 n4 v4)
                (Rec1 n1 v1) (Rec1 n2 v2) (Rec1 n3 v3) (Rec1 n4 v4)
  where
    splitIso = iso  (\(Rec2 a b) -> (Rec1 a, Rec1 b))
                    (\(Rec1 a, Rec1 b) -> Rec2 a b)

instance SplitIso (Rec3 n1 v1 n2 v2 n3 v3) (Rec3 n4 v4 n5 v5 n6 v6)
            (Rec2 n1 v1 n2 v2) (Rec1 n3 v3) (Rec2 n4 v4 n5 v5) (Rec1 n6 v6)
  where
    splitIso = iso  (\(Rec3 a b c) -> (Rec2 a b, Rec1 c))
                    (\(Rec2 a b, Rec1 c) -> Rec3 a b c)

instance SplitIso (Rec4 n1 v1 n2 v2 n3 v3 n4 v4) (Rec4 n5 v5 n6 v6 n7 v7 n8 v8)
        (Rec2 n1 v1 n2 v2) (Rec2 n3 v3 n4 v4) (Rec2 n5 v5 n6 v6) (Rec2 n7 v7 n8 v8)
  where
    splitIso = iso  (\(Rec4 a b c d) -> (Rec2 a b, Rec2 c d))
                    (\(Rec2 a b, Rec2 c d) -> Rec4 a b c d)

--instance SplitIso (Rec2 n1 v1 n2 v2) (Rec1 n1 v1) (Rec1 n2 v2) where
--    splitIso = iso (\(Rec2 a b) -> (Rec1 a, Rec1 b)) (\(Rec1 a, Rec1 b) -> Rec2 a b)

class FieldLens n a a' v v' | n a -> v, n a' -> v' where
    fieldLens :: Proxy n -> Lens a a' v v'

class FieldLensB b n a a' v v' | n a -> v, n a' -> v' where
    fieldLensB :: Proxy b -> Proxy n -> Lens a a' v v'

instance FieldLens n (Rec1 n1 v1) (Rec1 n1 v2) v1 v2 where
    fieldLens _ f (Rec1 v1) = fmap Rec1 $ f v1

instance (FieldLens n b b' v v', SplitIso a a' b c b' c', c~c')
    => FieldLensB True n a a' v v'
  where
    -- fieldLensB _ pn = splitIso . _1 . fieldLens pn . from splitIso
    fieldLensB _ n = withIso splitIso
        (\f g ->
            (\h a -> let (x,y) = f a in
                fmap (g . flip (,) y) $ fieldLens n h x
            )
        )

instance (FieldLens n c c' v v', SplitIso a a' b c b' c', b~b')
    => FieldLensB False n a a' v v'
  where
    -- fieldLensB _ pn = splitIso . _1 . fieldLens pn . from splitIso
    fieldLensB _ n = withIso splitIso
        (\f g ->
            (\h a -> let (x,y) = f a in
                fmap (g . (,) x) $ fieldLens n h y
            )
        )

instance (FieldLensB (n==n1) n (Rec2 n1 v1 n2 v2) (Rec2 n1 v3 n2 v4) v v')
    => FieldLens n (Rec2 n1 v1 n2 v2) (Rec2 n1 v3 n2 v4) v v'
  where
    fieldLens = fieldLensB (Proxy :: Proxy (n == n1))

instance (FieldLensB (n==n1||n==n2) n (Rec3 n1 v1 n2 v2 n3 v3) (Rec3 n1 v4 n2 v5 n3 v6) v v')
    => FieldLens n (Rec3 n1 v1 n2 v2 n3 v3) (Rec3 n1 v4 n2 v5 n3 v6) v v'
  where
    fieldLens = fieldLensB (Proxy :: Proxy (n == n1||n==n2))

instance (FieldLensB (n==n1||n==n2) n (Rec4 n1 v1 n2 v2 n3 v3 n4 v4) (Rec4 n1 v5 n2 v6 n3 v7 n4 v8) v v')
    => FieldLens n (Rec4 n1 v1 n2 v2 n3 v3 n4 v4) (Rec4 n1 v5 n2 v6 n3 v7 n4 v8) v v'
  where
    fieldLens = fieldLensB (Proxy :: Proxy (n == n1||n==n2))

getField pn a = a ^. fieldLens pn
setField pn v a = a & fieldLens pn .~ v
{-

instance (FieldLens n b b' v v', SplitIso a a' b c b' c')
    => FieldLensB True n a a' v v'
  where
    fieldLensB _ pn = l1 . fl . from si1
      where
        si1 = splitIso :: Iso a a' (b,c) (b',c')
        -- si2 = splitIso :: Iso' a' (b',c)
        fl = fieldLens pn :: Lens b b' v v'
        l1 = si1 . _1 :: Lens a (b',c) v v'
        {-
        l1 = withIso si1 (\f _ ->
                    \h x -> let (b,c) = f x in
                        fmap (flip (,) c) $ h b
                ) :: Lens a (b',c) v v'
            --si1 . _1 :: Lens a (b',c) v v'
        -}
-}
