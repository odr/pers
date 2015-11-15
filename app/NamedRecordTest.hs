{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module NamedRecordTest where

#define Four
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
type (<+) a b = (+>) a b
infixr 6 <+
#endif

type family Person where
-- type
#ifdef One
    Person = NamedRec   (() <+ "name":>String   -- NamedRecord
#else
#ifdef Four
    Person =    ("name":>String        -- NamedRecord2
#else
    Person =    (() <+ "name":>String        -- NamedRecord2
#endif
#endif
                            <+ "age":>Int
                            <+ "gender":>Bool
                            <+ "year":>Int
                            <+ "month":>Int
                            <+ "day":>Int
                            <+ "week":>Maybe Int
                            <+ "1":>Int
                            <+ "2":>Int
                            <+ "3":>Int
                            <+ "4":>Int
                            <+ "5":>Int
                            <+ "6":>Int
                            <+ "7":>Int
                            <+ "8":>Int
                            <+ "9":>Int
                            <+ "10":>Int
                            <+ "11":>Int
                            <+ "12":>Int
                            <+ "13":>Int
                            <+ "14":>Int
                            <+ "15":>Int
                            <+ "16":>Int
                            <+ "17":>Int
                            <+ "18":>Int
                            <+ "19":>Int
                            <+ "20":>Int
                            <+ "21":>Int
                            <+ "22":>Int
                            <+ "23":>Int
                            <+ "24":>Int
                            <+ "25":>Int
                            <+ "26":>Int
                            <+ "27":>Int
                            <+ "28":>Int
                            <+ "29":>Int
                            <+ "30":>Int
                            <+ "31":>Int
                            <+ "32":>Int
                            <+ "33":>Int
                            <+ "34":>Int
                            <+ "35":>Int
                            <+ "36":>Int
                            <+ "37":>Int
                            <+ "38":>Int
                            <+ "39":>Int
                            <+ "40":>Int
                            <+ "41":>Int
                            <+ "42":>Int
                            <+ "43":>Int
{-
                            <+ "44":>Int
                            <+ "45":>Int
                            <+ "46":>Int
                            <+ "47":>Int
                            <+ "48":>Int
                            <+ "49":>Int
-}
                             )
#ifdef Four

plPerson = Proxy :: Proxy Person
plPersonRev = Proxy :: Proxy (ListRev Person)
-- plNumsPair = Proxy :: Proxy (ListPair Nums)
plPersonTree = Proxy :: Proxy (ListTree Person)
-- ghci; time stack build:
-- 46 => 13s    ; 21.5
-- 23 => 2.05s  ; 11.3
-- 11 => 0.26s  ; 1.2
-- 56 => 23s    ; 35
-- type family PersonTree where PersonTree = ListTree Person
-- type family PersonMaybe where PersonMaybe = LiftedRec PersonTree Maybe
-- newPerson :: PersonMaybe
newPerson = newRec (Proxy :: Proxy (ListTree Person))
run = print newPerson
#else
#ifdef Three
type family PersonMaybe where PersonMaybe = TLift Person Maybe
newPerson :: PersonMaybe
#else
newPerson :: NewRec Person
#endif
newPerson = newRec (Proxy :: Proxy Person)

run = print newPerson
{-
person :: NewRec Person -> Either [String] Person
person = toRec

-}

#endif
