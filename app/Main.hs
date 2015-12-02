{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Default(Default(..))
import qualified Data.Map as M
import Database.Persist(toPersistValue)
import Lens.Micro
-- import Lens.Simple
-- import Control.Lens
import Data.Proxy(Proxy(..))

import NamedValue -- (Lifted)
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

meMap = M.fromList  [ (someSymbolVal "name"     , toPersistValue "Dima")
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

mePerson'' = fmap (\p -> p
                    & lw' .~ (Just 3)
                    & lp' . lw .~ (Just 5)
                    & lyan' .~ (V 18 +> V 1997 +> V "Lena")
                )
                mePerson' :: Either [SomeSymbol] Person'
  where
    lp' = fieldLens (Proxy :: Proxy ("person":>Person))
    lw  = fieldLens (Proxy :: Proxy  ("week" :> Maybe Int))
    lw' = fieldLens (Proxy :: Proxy ("week" :> Maybe Int))
    lyan' = recLens :: Lens' Person' ("year" :> Int +> "age" :>Int +> "name" :> String)
