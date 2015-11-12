{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module NamedRecordTest where

import NamedRecord
import Data.Typeable(Proxy(..))


type family Person where
-- type
    Person = NamedRec   (() <+ "name":>String
                            <+ "age":>Int
                            <+ "gender":>Bool
                            <+ "year":>Int
                            <+ "month":>Int
                            <+ "day":>Int
                            <+ "week":>Maybe Int
                        )
newPerson :: NewRec Person
newPerson = newRec (Proxy :: Proxy Person)

{-
person :: NewRec Person -> Either [String] Person
person = toRec


run = print newPerson
-}

