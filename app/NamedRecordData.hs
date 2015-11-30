{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module NamedRecordData where

import Data.Default(def)
import NamedRecord

type Person = "name":>String
            +> "age":>Int
            +> "gender":>Bool
            +> "year":>Int
            +> "month":>Int
            +> "day":>Int
            +> "week":>Maybe Int

type Long = "1":>Int
            +> "2":>Int
            +> "3":>Int
            +> "4":>Int
            +> "5":>Int
            +> "6":>Int
            +> "7":>Int
            +> "8":>Int
            +> "9":>Int
            +> "10":>Int
            +> "11":>Int
            +> "12":>Int
            +> "13":>Int
            +> "14":>Int
            +> "15":>Int
            +> "16":>Int
            +> "17":>Int
            +> "18":>Int
            +> "19":>Int
            +> "20":>Int
            +> "21":>Int
            +> "22":>Int
            +> "23":>Int
            +> "24":>Int
            +> "25":>Int
            +> "26":>Int
            +> "27":>Int
            +> "28":>Int
            +> "29":>Int
            +> "30":>Int
            +> "31":>Int
            +> "32":>Int
            +> "33":>Int
            +> "34":>Int
            +> "35":>Int
            +> "36":>Int
            +> "37":>Int
            +> "38":>Int
            +> "39":>Int
            +> "40":>Int
            +> "41":>Int
            +> "42":>Int
            +> "43":>Int
            +> "44":>Int
            +> "45":>Int
            +> "46":>Int
            +> "47":>Int
            +> "48":>Int
            +> "49":>Int
            +> "50":>Person

defPerson = def :: Lifted Maybe Person

defLong = def :: Lifted Maybe Long
