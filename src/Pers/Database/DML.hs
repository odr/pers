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
{-# LANGUAGE PolyKinds #-}
module Pers.Database.DML where

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
import Lens.Micro((^.))

import Pers.TH
import Pers.Types
import Pers.Database.DDL

class (TableLike a) => DML (rep::Rep) back a where
    -- | Insert the list of values into database.
    -- Should create Insert-statement with parameters
    -- and execute it for all values in list
    ins :: (MonadIO m, MonadMask m) => Proxy '(rep,a) -> [VRec rep (RecordDef a)]
        -> SessionMonad back m ()
    -- | Simple update by pk. Return list of pk which were updated
    upd :: (MonadIO m, MonadMask m) => Proxy '(rep,a) -> [VRec rep (RecordDef a)]
        -> SessionMonad back m [VRec rep (Key a)]
    -- | Delete values by condition.
    -- Count of deleted records is returned
    del :: (MonadIO m, MonadMask m)
        => Proxy '(rep,a) -> Cond rep back (RecordDef a) -> SessionMonad back m Int
    selProj :: (MonadIO m, MonadMask m
            , (NamesMinus b (RecordDef a)) ~ '[]
            , Names b
            , RowRepDDL rep back (ProjNames (RecordDef a) b)
            , Names ((NRec (RecordDef a) :\\ KeyDef a) :\\ b)
            )
        => Proxy '(rep,a,b) -> Cond rep back (RecordDef a)
        -> SessionMonad back m [VRec rep (ProjNames (RecordDef a) b)]

-- | Select values by condition
sel (_::Proxy '(rep, a))
    = selProj (Proxy :: Proxy '(rep,a,NRec (RecordDef a)))

upsert (p :: Proxy '(rep,a)) (xs::[VRec rep (RecordDef a)]) = do
    res <- upd p xs
    ins p $ filter (\x -> not $ (x ^. lensPk p) `elem` res) xs

-- | In many cases PK should be generated.
-- There are some possibilities:
--
-- * autogenerated PK (Sqlite, MSSQL)
-- * generated from sequence (Oracle, PostgreSQL(?)) then inserted
--
-- In all cases interface is the same.
-- If we need sequence name (Oracle) we can derive it from table name.
class (TableLike a) => InsAutoPK (rep::Rep) back a where
    insAuto :: (MonadIO m, MonadMask m)
            => Proxy '(rep,a) -> [VRec rep (DataRecord a)]
            -> SessionMonad back m [VRec rep (Key a)]

insRecCmd :: (KnownSymbol t, RowRepDDL rep back r, Names (NRec r), DBOption back)
    => Proxy '(rep,back,t,r) -> Text
insRecCmd (_ :: Proxy '(rep,back,t,r))
    = format "INSERT INTO {} ({}) VALUES({})"
        ( symbolVal' (proxy# :: Proxy# t)
        , TL.intercalate "," $ map TL.pack ns
        , TL.intercalate ","
            $ zipWith (\n -> const $ paramName (proxy# :: Proxy# back) n)
                        [1..] ns
        )
  where
    ns = names (proxy# :: Proxy# (NRec r))

insRecCmdPars :: (KnownSymbol t, RowRepDDL rep back r, Names (NRec r), DBOption back)
    => Proxy '(rep,back,t,r) -> [VRec rep r] -> (Text, [[FieldDB back]])
insRecCmdPars (p::Proxy '(rep,back,t,r)) rs
    = (insRecCmd p, map (rowDb (proxy# :: Proxy# '(rep,back)) (Proxy::Proxy r)) rs)

updRecCmdPars
    :: (KnownSymbol (TabName t)
        , RowRepDDL rep back (DataRecord t)
        , RowRepDDL rep back (Key t)
        , RecLens rep (RecordDef t) (Key t)
        , RecLens rep (RecordDef t) (DataRecord t)
        , DBOption back
        , (Key t :\\ RecordDef t) ~ '[]
        , Names (NRec (Key t))
        , Names (NRec (DataRecord t))
        , Single rep
        )
    => Proxy '(rep,back,t) -> [VRec rep (RecordDef t)] -> (Text, [[FieldDB back]])
updRecCmdPars (_ :: Proxy '(rep,back,t)) [] = mempty
updRecCmdPars (proxy :: Proxy '(rep,back,t)) (rec:recs)
    =   ( format "UPDATE {} SET {} WHERE {}"
            ( symbolVal' (proxy# :: Proxy# (TabName t))
            , TL.intercalate ","
                $ zipWith (\n s ->
                        format "{} = {}" (s, paramName (proxy# :: Proxy# back) n)
                    ) [1..] ns
            , w
            )
        , (dataDb rec ++ p) : map ((++) <$> dataDb <*> keyDb) recs
        )
  where
    ns = names (proxy# :: Proxy# (NRec (DataRecord t)))
    (w,_,p) = runRWS (sqlWhere $ cond key) () (length ns + 1)
      where
        cond r = Equal (Proxy :: Proxy (Key t)) r :: Cond rep back (RecordDef t)
        key = rec ^. recLens (proxy#::Proxy# '(rep,RecordDef t,Key t))
    keyDb   = recDbPk   proxy
    dataDb  = recDbData proxy

class AutoGenPK back a where
    getPK :: (MonadIO m) => SessionMonad back m a

data Cond (rep::Rep) back (a :: [(Symbol,*)])
    = forall b. (Names (NRec b), RowRepDDL rep back b, (b :\\ a) ~ '[])
        => Equal (Proxy b) (VRec rep b)
    | forall s b. (KnownSymbol s, RowRepDDL rep back '[s:::b], Elem (s:::b) a ~ True)
        => In (Proxy '(rep,back,s)) [b]
    | forall b. (HasDef (VRec rep (ProjNames a b)) ~ True
                , Names b, (b :\\ NRec a) ~ '[])
        => Null (Proxy b) -- | All fields in subrecord is null
    | forall b. (HasDef (VRec rep (ProjNames a b)) ~ True
                , Names b, (b :\\ NRec a) ~ '[])
        => NotNull (Proxy b) -- | All fields in subrecord is not null
    | forall b. (Names (NRec b), RowRepDDL rep back b, (b :\\ a) ~ '[])
        => Great (Proxy b) (VRec rep b)
    | forall b. (Names (NRec b), RowRepDDL rep back b, (b :\\ a) ~ '[])
        => Least (Proxy b) (VRec rep b)
    | And [Cond rep back a]
    | Or  [Cond rep back a]
    | Not (Cond rep back a)
    | CondTrue

instance Monoid (Cond rep back a) where
    mempty = CondTrue
    c1 `mappend` c2 = And [c1,c2]
    mconcat = And

sqlWhere :: (DBOption back, Single rep)
    => Cond rep back a -> RWS () [FieldDB back] Int Text
sqlWhere (x :: Cond rep back a) = case x of
    CondTrue        -> return "1=1"
    Equal pb b      -> rel pb b "="
    In (ps :: Proxy '(rep,back,s)) (bs :: [b]) ->
        fmap  ( format "{} IN ({})"
              . ((,) $ symbolVal' (proxy# :: Proxy# s))
              . (TL.intercalate ", ")
              )
            $ mapM (\b -> do
                    num <- get
                    tell $ rowDb (proxy# :: Proxy# '(rep,back))
                                (Proxy :: Proxy '[s:::b]) (single (proxy# :: Proxy# rep) b)
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
    rel :: (Names (NRec b), RowRepDDL rep back b)
        => Proxy b -> VRec rep b -> Text -> RWS () [FieldDB back] Int Text
    rel (pb :: Proxy b) vb op = do
        let bns = names (proxy# :: Proxy# (NRec b))
        num <- get
        tell $ rowDb (proxy# :: Proxy# '(rep,back)) pb vb
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
getSqlWhere :: (DBOption back, Single rep) => Cond rep back a -> (Text, [FieldDB back])
getSqlWhere c = let (r,_,w) = runRWS (sqlWhere c) () 1 in (r,w)

selRecCmdPars :: (KnownSymbol t, Single rep, RowRepDDL rep back r, Names a, DBOption back)
    => Proxy '(rep,t,a) -> Cond rep back (r::[(Symbol,*)])
            -> (Text,[FieldDB back])
selRecCmdPars (p::Proxy '(rep,t,a)) c =
    ( format "SELECT {} FROM {} WHERE {}"
        ( TL.intercalate "," $ map TL.pack ns
        , symbolVal' (proxy# :: Proxy# t)
        , w
        )
    , ps
    )
  where
    (w,ps) = getSqlWhere c
    ns = names (proxy# :: Proxy# a)

delRecCmdPars :: (Single rep, KnownSymbol t, DBOption back)
    => Proxy# (t::Symbol) -> Cond rep back r -> (Text,[FieldDB back])
delRecCmdPars pt c =
    ( format "DELETE FROM {} WHERE {}" ( symbolVal' pt, w )
    , ps
    )
  where
    (w,ps) = getSqlWhere c


