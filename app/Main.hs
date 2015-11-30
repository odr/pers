module Main where

import Data.Default(Default(..))
import Data.Text(Text)
import qualified Data.Map as M
import NamedRecord -- (Lifted)
import NamedRecordData -- (Person, defPerson)

main :: IO ()
--main = print (maybeToRec defLong1 :: Either [String] Long)
main = print (toRec defLong1 :: Either [Text] Long)

defPerson1 = defPerson -- :: Lifted Maybe Person  -- 8s
defPerson2 = defPerson -- :: Lifted Maybe Person -- 11s
bp = defPerson1 == defPerson2
-- 30s (with If) -> 11,5s (without If) -> 10,5 (Cnt -> EqCnt) -> 13s (Bool::+Clear)

defLong1 = def::Lifted Maybe Long

{-
meMap = M.fromList [("name", FV "Dmitry"),("age",FV 46),("gender",FV True)
                   ,("year",FV 1969),("month",FV 6),("day",FV 13),("week",FV 6)
                   ]

mePerson = fromMap meMap

-}
