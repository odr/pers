module Main where

import Data.Default(Default(..))
import NamedRecord -- (Lifted)
import NamedRecordData -- (Person, defPerson)

main :: IO ()
main = print (maybeToRec defLong1 :: Either [String] Long)

defPerson1 = defPerson -- :: Lifted Maybe Person  -- 8s
defPerson2 = defPerson -- :: Lifted Maybe Person -- 11s
bp = defPerson1 == defPerson2
-- 30s (with If) -> 11,5s (without If) -> 10,5 (Cnt -> EqCnt) -> 13s (Bool::+Clear)

defLong1 = def::Lifted Maybe Long
