module Main where

import Data.Default(Default(..))
-- import Data.Text(Text)
import qualified Data.Map as M
import NamedRecord -- (Lifted)
import NamedRecordData -- (Person, defPerson)
import FromMap
import GHC.TypeLits(SomeSymbol, someSymbolVal)

main :: IO ()
--main = print (maybeToRec defLong1 :: Either [String] Long)
main = print (toRec defLong1 :: Either [SomeSymbol] Long)

defPerson1 = defPerson -- :: Lifted Maybe Person  -- 8s
defPerson2 = defPerson -- :: Lifted Maybe Person -- 11s
bp = defPerson1 == defPerson2
-- 30s (with If) -> 11,5s (without If) -> 10,5 (Cnt -> EqCnt) -> 13s (Bool::+Clear)

defLong1 = def::Lifted Maybe Long

{-
meMap = M.fromList  [ (someSymbolVal "name"     , FV "Dmitry")
                    , (someSymbolVal "age"      , FV 46)
                    , (someSymbolVal "gender"   , FV True)
                    , (someSymbolVal "year"     , FV 1969)
                    , (someSymbolVal "month"    , FV 6)
                    , (someSymbolVal "day"      , FV 13)
                    , (someSymbolVal "week"     , FV 6)
                   ]

mePerson = fromMap meMap
-}
