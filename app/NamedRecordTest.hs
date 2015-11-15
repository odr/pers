{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module NamedRecordTest where

#define Three
import Data.Typeable(Proxy(..))

#ifdef One
-- 13 s build
import NamedRecord
#endif
#ifdef Two
-- 13 s build
import NamedRecord2
#endif
#ifdef Three
-- 13 s build
import NamedRecord3
#endif
#ifdef Four
-- 13 s build
import NamedRecord4
#endif

type family Person where
-- type
#ifdef One
    Person = NamedRec   (() <+ "name":>String   -- NamedRecord
#else
    Person =    (() <+ "name":>String        -- NamedRecord2
#endif
                            <+ "age":>Int
                            <+ "gender":>Bool
                            <+ "year":>Int
                            <+ "month":>Int
                            <+ "day":>Int
                            <+ "week":>Maybe Int
                        )
#ifdef Three
type family PersonMaybe where PersonMaybe = TLift Person Maybe
-- newPerson :: PersonMaybe
#else
-- newPerson :: NewRec Person
#endif
newPerson = newRec (Proxy :: Proxy Person)

run = print newPerson
{-
person :: NewRec Person -> Either [String] Person
person = toRec

-}


