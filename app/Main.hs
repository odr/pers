{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Default(Default(..))
import qualified Data.Map as M
import Database.Persist(toPersistValue)
import Lens.Micro
-- import Lens.Simple
-- import Control.Lens
import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Data.Int(Int64)

-- import NamedValue -- (Lifted)
import NamedRecord -- (Lifted)
import NamedRecordData -- (Person, defPerson)
import PersistRec
import DDL
import Sqlite
import GHC.TypeLits(SomeSymbol, someSymbolVal)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.Trans.Reader(runReaderT)
import Control.Monad.IO.Class(MonadIO(..))

import           Database.SQLite3 -- .Simple
-- import           Database.SQLite.Simple.FromRow
import qualified Data.Text as T
import DML

{-
data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)
-}
type TRec1 = "id" :> Int64 +> "name" :> T.Text +> "val" :> Maybe Double
type Tab1 = Table "tab1" TRec1 ("id" :> Int64)
pTab1 = Proxy :: Proxy Tab1
setRec1 :: TRec1 -> TRec1
setRec1 = id

sql :: IO ()
sql = do
    conn <- open "test.db"
  -- exec conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  -- exec conn "CREATE TABLE IF NOT EXISTS test2 (id INTEGER, str TEXT, PRIMARY KEY (id))"
  --TIO.putStrLn $ createTable (proxy# :: Proxy# Sqlite) (Proxy :: Proxy Tab1)
  --exec conn $ TL.toStrict $
    pk <- runReaderT (do
            dropTable sqlite (Proxy :: Proxy Tab1)
            liftIO $ print 1
            createTable sqlite (Proxy :: Proxy Tab1)
            liftIO $ print 2
            let rec1    =  (V 1         :: "id"     :> Int64       )
                        +> (V "text1"   :: "name"   :> T.Text      )
                        +> (V Nothing   :: "val"    :> Maybe Double)
            ins sqlite (Table rec1 :: Tab1)
            liftIO $ print 3
            let rec2 = setRec1 $ V 2 +> V "text2" +> V (Just 2.2)
            ins sqlite (Table rec2 :: Tab1)
            liftIO $ print 4
            let lensId = fieldLens (Proxy :: Proxy ("id" :>  Int64))
            ins sqlite (Table $ rec1 & lensId .~ 3 :: Tab1)
            let lensIdName = recLens :: Lens' TRec1 ("id" :> Int64 +> "name" :> T.Text)
            ins sqlite (Table $ rec2 & lensIdName .~ (V 4 +> V "text4") :: Tab1)
            insAuto sqlite pTab1
                (  (V "text auto 1" :: "name"   :> T.Text)
                +> (V $ Just 1.1    :: "val"    :> Maybe Double)
                :: DataRow Tab1
                )

            -- TODO обработка ошибок
            -- TODO избавление от параметра sqlite
            -- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом

            -- ins sqlite

        ) conn
    print pk
  -- TIO.putStrLn $ ins (proxy# :: Proxy# Sqlite) (Proxy :: Proxy Tab1)

  {-
  i1 <- prepare conn "INSERT INTO test (str) VALUES (?1)"
  bind i1 [SQLText "test new 2"]
  step i1
  finalize i1
  -- exec conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  i2 <- prepare conn "INSERT INTO test (id, str) VALUES (?1,?2)"
  bind i2 [SQLInteger 1113, SQLText "test new 3"]
  step i2
  finalize i2
  rowId <- lastInsertRowId conn
  print rowId
  -- executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
  i3 <- prepare conn "UPDATE test SET str = ?1 WHERE id = ?2"
  bind i3 [SQLText "updated str", SQLInteger rowId]
  step i3
  finalize i3
  execPrint conn "SELECT * from test"
  -- mapM_ print r
  i4 <- prepare conn "DELETE FROM test WHERE id = ?1"
  bind i4 [SQLInteger rowId]
  step i4
  finalize i4
  -}
    close conn


main :: IO ()
main = do
    sql
{-
-- {-
    print (toRec defLong1 :: Either [SomeSymbol] Long)
-- -}
    print defPerson
-- {-
    print defLong
    print bp
-- -}
    print meMap
-- {-
    print mePerson
-- -}
-- {-
    print mePerson'
    print mePerson''
-- -}
-- {-
    print testAssoc
    print (names (proxy# :: Proxy# Long))
-- -}
-}
{-
-- {-
defPerson1 = defPerson
defPerson2 = defPerson
bp = defPerson1 == defPerson2
-- -}
defLong1 = def::Lifted Maybe Long

meMap = M.fromList  [ (someSymbolVal "name"     , toPersistValue ("Dima"::String))
                    , (someSymbolVal "age"      , toPersistValue (46::Int))
                    , (someSymbolVal "gender"   , toPersistValue True)
                    , (someSymbolVal "year"     , toPersistValue (1969::Int))
                    , (someSymbolVal "month"    , toPersistValue (6::Int))
                    , (someSymbolVal "day"      , toPersistValue (13::Int))
                    -- , (someSymbolVal "week"     , toPersistValue (6::Int))
                   ]

-- {-
mePerson = mapToRec pPerson meMap
-- -}

-- {-
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
-- -}

-- {-
testAssoc
    = (V 5 +> V "str" +> V 1 +> V 7 :: "a":>Int +> "b":>String +> "c":>Int +>"d":>Int)
    == (V 5 +> (V "str" +> V 1) +> V 7 :: "a":>Int +> ("b":>String +> "c":>Int +>"d":>Int))
-- -}
-}
