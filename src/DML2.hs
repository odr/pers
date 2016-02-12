{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DML2 where

import GHC.Prim(Proxy#, proxy#)
import Data.Proxy(Proxy(..))
import Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Trans.RWS(RWS(..), get, tell, put, runRWS)
import Control.Monad.Catch
import GHC.TypeLits
import Data.Type.Equality -- (type (==))
import Data.Type.Bool -- (type (||))
import Data.Text.Format(format, Only(..))
import Data.Promotion.Prelude.List
import Data.List(intercalate)

import NRTH
import NamedRecord2
import DDL2

class DML back a where
    -- | Insert the list of values into database.
    -- Should create Insert-statement with parameters
    -- and execute it for all values in list
    ins :: (MonadIO m, MonadMask m) => [a] -> SessionMonad back m ()
--    upd :: (MonadIO m, MonadMask m) => [a] -> SessionMonad back m ()
{-
    -- | Select part of values by condition
    selProj :: (MonadIO m, MonadMask m)
        => forall b. Has a b ~ True => Proxy a -> Maybe (Cond back (Record a))
                -> SessionMonad back m [Record b]
    -- | update values with
    upd :: (MonadIO m, MonadMask m, Has a b)
        => Proxy a -> b -> Maybe (Cond back (Record a))
                -> SessionMonad back m Int
-}
    -- | Delete values by condition.
    -- Count of deleted records is returned
    del :: (MonadIO m, MonadMask m)
        => Proxy a -> Cond back (RecordDef a) -> SessionMonad back m Int
    -- | Select values by condition
    sel :: (MonadIO m, MonadMask m) -- , Has (Record a) (Proxy (Record a)) ~ True)
        => Proxy a -> Cond back (RecordDef a) -> SessionMonad back m [VRecRep (RecordDef a)]
    -- sel (pa :: Proxy a) c = selProj pa (Proxy :: Proxy (Record a)) c
    selProj :: (MonadIO m, MonadMask m
            , (b :\\ (NRec (RecordDef a))) ~ '[]
            , Names b
            , RowDDL back (ProjNames (RecordDef a) b))
        => Proxy a -> Proxy b -> Cond back (RecordDef a)
        -> SessionMonad back m [VRecRep (ProjNames (RecordDef a) b)]

-- | In many cases PK should be generated.
-- There are some possibilities:
--
-- * autogenerated PK (Sqlite, MSSQL)
-- * generated from sequence (Oracle, PostgreSQL(?)) then inserted
--
-- In all cases interface is the same.
-- If we need sequence name (Oracle) we can derive it from table name.
-- (Table name should be connected with 'DataRecord a')
class InsAutoPK back a where
    insAuto :: (MonadIO m, MonadMask m)
            => Proxy a -> [VRecRep (DataRecordDef a)]
            -> SessionMonad back m [VRecRep (ProjNames (RecordDef a) (KeyDef a))]

insRecCmd :: (KnownSymbol t, RowDDL back r, Names (NRec r), DBOption back)
    => Proxy# back -> Proxy# (t::Symbol) -> Proxy r -> Text
insRecCmd pb pt (_ :: Proxy r)
    = format "INSERT INTO {} ({}) VALUES({})"
        ( symbolVal' pt
        , TL.intercalate "," $ map TL.pack ns
        , TL.intercalate "," $ zipWith (\n -> const $ paramName pb n) [1..] ns
        )
  where
    ns = names (proxy# :: Proxy# (NRec r))

{-
updRecCmdPars :: (KnownSymbol t, RowDDL back r, NamesList r, DBOption back)
    => Proxy# back -> Proxy# (t::Symbol) -> Proxy# pk -> [r] -> (Text, [[FieldDB back]])
updRecCmdPars _ _ _ [] = mempty
updRecCmdPars pb pt (ppk :: Proxy# pk) ((r:rs) :: [r])
    = format "UPDATE {} SET {} WHERE {}"
        ( symbolVal' pt
        , TL.intercalate ","
            $ zipWith
                (\num name -> format "{} = {}" (TL.pack name, paramName pb num))
                [1..] ns
        , TL.intercalate "," $ zipWith (\n -> const $ paramName pb n) [1..] ns
        )
  where
    ns = namesStrL (proxy# :: Proxy# r)
    (w,ps) = getSqlWhere $ Equal $ rs ^. (recLens :: Lens' r pk)
-}
insRecCmdPars :: (KnownSymbol t, RowDDL back r, Names (NRec r), DBOption back)
    => Proxy# back -> Proxy# (t::Symbol) -> Proxy r -> [VRecRep r] -> (Text, [[FieldDB back]])
insRecCmdPars pb pt pr rs = (insRecCmd pb pt pr, map (rowDb pb pr) rs)

class AutoGenPK back a where
    getPK :: (MonadIO m) => SessionMonad back m a

data Cond back (a :: [(Symbol,*)])
    = forall b. (Names (NRec b), RowDDL back b, Minus b a ~ '[])
        => Equal (Proxy b) (VRecRep b)
    | forall s b. (KnownSymbol s, RowDDL back '[s:::b], Minus '[s:::b] a ~ '[])
        => In (Proxy s) [b]
    | forall b. (HasDef (VRecRep (ProjNames a b)) ~ True
                , Names b, Minus b (NRec a) ~ '[])
        => Null (Proxy b) -- | All fields in subrecord is null
    | forall b. (HasDef (VRecRep (ProjNames a b)) ~ True
                , Names b, Minus b (NRec a) ~ '[])
        => NotNull (Proxy b) -- | All fields in subrecord is not null
    | forall b. (Names (NRec b), RowDDL back b, Minus b a ~ '[])
        => Great (Proxy b) (VRecRep b)
    | forall b. (Names (NRec b), RowDDL back b, Minus b a ~ '[])
        => Least (Proxy b) (VRecRep b)
    | And [Cond back a]
    | Or  [Cond back a]
    | Not (Cond back a)
    | CondTrue

instance Monoid (Cond back a) where
    mempty = CondTrue
    c1 `mappend` c2 = And [c1,c2]
    mconcat = And

sqlWhere :: (DBOption back) => Cond back a -> RWS () [FieldDB back] Int Text
sqlWhere (x :: Cond back a) = case x of
    CondTrue        -> return "1=1"
    Equal pb b      -> rel pb b "="
    In (ps :: Proxy s) (bs :: [b]) ->
        fmap  ( format "{} IN ({})"
              . ((,) $ symbolVal' (proxy# :: Proxy# s))
              . (TL.intercalate ", ")
              )
            $ mapM (\b -> do
                    num <- get
                    tell $ rowDb (proxy# :: Proxy# back)
                                (Proxy :: Proxy '[s:::b]) (b,())
                    put $ num + 1
                    return $ paramName (proxy# :: Proxy# back) num
                ) bs
    Null pb         -> isNull pb ""
    NotNull pb      -> isNull pb "NOT"
    Great pb b      -> rel pb b ">"
    Least pb b      -> rel pb b "<"
    And cs          -> ao cs " AND "
    Or cs           -> ao cs " OR "
    Not c           -> fmap (format "NOT ({})" . Only) $ sqlWhere c
  where
    rel :: (Names (NRec b), RowDDL back b)
        => Proxy b -> VRecRep b -> Text -> RWS () [FieldDB back] Int Text
    rel (pb :: Proxy b) vb op = do
        let bns = names (proxy# :: Proxy# (NRec b))
        num <- get
        tell $ rowDb (proxy# :: Proxy# back) pb vb
        put $ num + length bns
        return
            $ TL.intercalate " AND "
            $ zipWith   (\n p ->
                    format "{} {} {}" (n,op,paramName (proxy# :: Proxy# back) p)
                ) bns [num..]
    isNull (_ :: Proxy b) (t::Text) = return
        $ TL.intercalate " AND "
        $ map (\n -> format "{} IS {} NULL" (n,t))
        $ names (proxy# :: Proxy# b)
    ao cs t = fmap (TL.intercalate t . map (format "({})" . Only)) $ mapM sqlWhere cs

-- | Convert Condition to pair: "Where-text" and "list of query-parameters".
getSqlWhere :: (DBOption back) => Cond back a -> (Text, [FieldDB back])
getSqlWhere c = let (r,_,w) = runRWS (sqlWhere c) () 1 in (r,w)

selRecCmdPars :: (KnownSymbol t, RowDDL back r, Names a, DBOption back
                , (Minus a (NRec r)) ~ '[])
    => Proxy# (t::Symbol) -> Proxy# a -> Cond back r -> (Text,[FieldDB back])
selRecCmdPars pt pa c =
    ( format "SELECT {} FROM {} WHERE {}"
        ( TL.intercalate "," $ map TL.pack ns
        , symbolVal' pt
        , w
        )
    , ps
    )
  where
    (w,ps) = getSqlWhere c
    ns = names pa

delRecCmdPars :: (KnownSymbol t, RowDDL back r, DBOption back)
    => Proxy# (t::Symbol) -> Cond back r -> (Text,[FieldDB back])
delRecCmdPars pt c =
    ( format "DELETE FROM {} WHERE {}" ( symbolVal' pt, w )
    , ps
    )
  where
    (w,ps) = getSqlWhere c



