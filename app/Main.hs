{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Default(Default(..))
import qualified Data.Map as M
import Database.Persist
import Lens.Micro
-- import Lens.Simple
-- import Control.Lens
import Data.Proxy(Proxy(..))

import NamedRecord -- (Lifted)
import NamedRecordData -- (Person, defPerson)
import PersistRec
import GHC.TypeLits(SomeSymbol, someSymbolVal)

main :: IO ()
main = do
    print (toRec defLong1 :: Either [SomeSymbol] Long)
    print defPerson
    print defLong
    print bp
    print meMap
    print mePerson
    print mePerson'
    print mePerson''


defPerson1 = defPerson
defPerson2 = defPerson
bp = defPerson1 == defPerson2

defLong1 = def::Lifted Maybe Long

meMap = M.fromList  [ (someSymbolVal "name"     , toPersistValue "Dmitry")
                    , (someSymbolVal "age"      , toPersistValue (46::Int))
                    , (someSymbolVal "gender"   , toPersistValue True)
                    , (someSymbolVal "year"     , toPersistValue (1969::Int))
                    , (someSymbolVal "month"    , toPersistValue (6::Int))
                    , (someSymbolVal "day"      , toPersistValue (13::Int))
                    -- , (someSymbolVal "week"     , toPersistValue (6::Int))
                   ]

mePerson = mapToRec pPerson meMap

type Person' = Person +> "person" :> Person

mePerson' = fmap (\p -> p +> V p :: Person') mePerson

mePerson'' = fmap (\p -> p & lp . lw .~ Just 5 & lw1 .~ Just 3) mePerson'
  where
    lp = fldLens (Proxy :: Proxy ("person":>Person))
    lw = fldLens (Proxy :: Proxy ("week" :> Maybe Int))
    lw1 = fldLens (Proxy :: Proxy ("week" :> Maybe Int))
