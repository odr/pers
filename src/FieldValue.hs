{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module FieldValue
    ( FieldValue(..)
    , ToRec(..)
    ) where

import Data.Text(Text)
import Data.Type.Equality((:~:)(..), testEquality)
import Data.Proxy(Proxy(..))


class FieldValue fv where
    fromFieldValue  :: forall b. fv -> Maybe b
    toFieldValue    :: forall b. b -> Maybe fv


-- data SomeFieldValue fv = forall b. FieldValue fv b => SomeFieldValue b

-- from persistent package. Copied to be undependent
data PersistValue
    = PersistText Text
    {-
    | PersistByteString ByteString
    | PersistInt64 Int64
    | PersistDouble Double
    | PersistRational Rational
    | PersistBool Bool
    | PersistDay Day
    | PersistTimeOfDay TimeOfDay
    | PersistUTCTime UTCTime
    | PersistNull
    | PersistList [PersistValue]
    | PersistMap [(Text, PersistValue)]
    | PersistObjectId ByteString -- ^ Intended especially for MongoDB backend
    | PersistDbSpecific ByteString
    -}

instance FieldValue PersistValue where
    fromFieldValue (PersistText t                    )
        = case testEquality (Proxy :: Proxy b) (Proxy :: Proxy Text) of
            Just (Refl :: b :~: Text) -> Just t
            _ -> Nothing
{-
    fromFieldValue (PersistByteString ByteString     )
    fromFieldValue (PersistInt64 Int64               )
    fromFieldValue (PersistDouble Double             )
    fromFieldValue (PersistRational Rational         )
    fromFieldValue (PersistBool Bool                 )
    fromFieldValue (PersistDay Day                   )
    fromFieldValue (PersistTimeOfDay TimeOfDay       )
    fromFieldValue (PersistUTCTime UTCTime           )
    fromFieldValue (PersistNull                      )
    fromFieldValue (PersistList [PersistValue]       )
    fromFieldValue (PersistMap [(Text, PersistValue)])
    fromFieldValue (PersistObjectId ByteString       )
    fromFieldValue (PersistDbSpecific ByteString     )
-}
class ToRec a where
    toRec :: Lifted Maybe a -> Either [SomeSymbol] a
    fromMap :: (FieldValue fv)
        => Proxy a -> Map SomeSymbol fv -> Lifted Maybe a

instance (KnownSymbol n) => ToRec (n:>v) where
    toRec (V mv) = maybe (Left [SomeSymbol (Proxy::Proxy n)])
                        (Right . V) mv
    fromMap _ m = maybe (V Nothing) (V . Just)
                $ M.lookup (SomeSymbol (Proxy::Proxy n)) m >>= fromFieldValue

instance (ToRec a, ToRec b) => ToRec (a,b) where
    toRec (a,b) = case (toRec a, toRec b) of
        (Right x, Right y) -> Right (x,y)
        (x, y) -> Left $ concat $ lefts [x] ++ lefts [y]
    fromMap _ m
        = (fromMap (Proxy :: Proxy a) m, fromMap (Proxy :: Proxy b) m)

mapToRec :: (ToRec a, FieldValue fv)
        => Proxy a -> Map SomeSymbol fv -> Either [SomeSymbol] a
mapToRec p = toRec . fromMap p

