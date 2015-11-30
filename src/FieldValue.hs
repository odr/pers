{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module FieldValue where

class FieldValue fv where
    fromFieldValue  :: forall b. fv -> Maybe b
    toFieldValue    :: forall b. b -> Maybe fv


-- data SomeFieldValue fv = forall b. FieldValue fv b => SomeFieldValue b
