{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Pers.Servant.Servant where

import Data.Proxy(Proxy(..))
import GHC.Prim(Proxy#, proxy#)
import Servant
import Control.Monad.Trans.Either
-- import Control.Monad.Except
import Data.Aeson(ToJSON(..),FromJSON(..), encode)
import Servant.HTML.Lucid(HTML)
import Servant.Docs
import Servant.JQuery
import Data.Tagged
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Int(Int64)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import GHC.TypeLits(Symbol(..), KnownSymbol, SomeSymbol(..), symbolVal', symbolVal)
import Data.Type.Bool
import Data.Typeable
import Data.Maybe(listToMaybe)
import Data.Bifunctor(bimap)
import Control.Applicative(liftA2)
import Control.Concurrent(forkIO)

import Pers.Types
import Pers.Database.DDL
import Pers.Database.DML

-- type PersMonad back = SessionMonad back (ExceptT ServantErr IO)
type PersMonad back = SessionMonad back (EitherT ServantErr IO)

type ServData rep x
    = '(x, VRec rep (RecordDef x), VRec rep (Key x), VRec rep (DataRecord x))
-- type family ServDatas rep xs where
--     ServDatas rep '[] = '[]
--     ServDatas rep (x ': xs) = ServData rep x ': ServDatas rep xs

type PersAPI' rep opt back x
    = PersAPI rep opt back x
        (VRec rep (RecordDef x)) (VRec rep (Key x)) (VRec rep (DataRecord x))

class   ( TableLike a
        , Rep rep (RecordDef a) ar
        , Rep rep (Key a) kr
        , Rep rep (DataRecord a) dr
        )
        => PersServant (rep::R) (opt:: *) (back:: *) (a::DataDef *) (ar:: *) (kr:: *) (dr:: *)
                | rep a -> ar, rep a -> kr, rep a -> dr where
    type PersListAPI rep opt back a ar kr dr
    type PersRecAPI rep opt back a ar kr dr
    type PersAPI rep opt back a ar kr dr
    persServer :: Proxy# rep -> Proxy# opt -> Proxy# back -> Proxy '(a,ar,kr,dr)
        -> ServerT (PersAPI rep opt back a ar kr dr) (PersMonad back)
    jsPersAPI :: Proxy '(rep, opt, back, '(a, ar, kr, dr)) -> T.Text

toPersAPI :: Proxy '(rep, opt, back, '(a, ar, kr, dr))
        -> Proxy (PersAPI rep opt back a ar kr dr)
toPersAPI _ = Proxy

mkJsAPI ::  ( HasJQ (PersAPI rep opt back a ar kr dr)
            , PersServant rep opt back a ar kr dr
            , GenerateCode (JQ (PersAPI rep opt back a ar kr dr))
            , KnownSymbol (TabName a)
            )
        => Proxy '(rep, opt, back, '(a, ar, kr, dr)) -> IO ()
mkJsAPI (ps :: Proxy '(rep, opt, back, '(a, ar, kr, dr)))
    = mapM_ (\(n,s) -> forkIO $ TIO.writeFile (mconcat ["js/", sTab, n]) s)
        [ ("api.js", T.pack $ jsForAPI pAPI)
        , (".js", jsPersAPI ps)
        ]
  where
    pAPI = toPersAPI ps
    sTab = symbolVal' (proxy# :: Proxy# (TabName a))

type PRecs rep rec ar = Tagged ('(rep, rec)) [ar]
type PPks rep rec pk kr = Tagged ('(rep, ProjNames rec pk)) [kr]

instance    ( DBOption back
            , DML rep back (TableDef n rec pk uk fk) ar kr dr
            , ContainNames rec (NRec rec)
            , ContainNames rec pk
            , Names (NRec rec)
            , Names pk
            , RowRepDDL rep back (ProjNames rec (NRec rec)) ar
            , RowRepDDL rep back (ProjNames rec pk) kr
            , PersServantIns (IsAutoPKb rep back kr) rep back
                             (TableDef n rec pk uk fk) ar kr dr
            , Curring rep kr
            , ServerT   (CaptureN pk kr (Get '[HTML, JSON]
                            (Tagged opt (Tagged '(rep, (TableDef n rec pk uk fk)) (Maybe ar))))
                        )
                        (PersMonad back)
                ~ Curried rep kr
                        (PersMonad back
                            (Tagged opt (Tagged '(rep, (TableDef n rec pk uk fk)) (Maybe ar)))
                        )
            )
    => PersServant (rep::R) opt back (TableDef n rec pk uk fk) ar kr dr
  where
    type PersListAPI rep opt back ((TableDef n rec pk uk fk)) ar kr dr =
        n :> Get '[HTML,JSON]
                (Tagged opt (Tagged '(rep, back, TableDef n rec pk uk fk, ar, kr, dr) [ar]))
    type PersRecAPI rep opt back (TableDef n rec pk uk fk) ar kr dr =
        n   :> "rec"
            :> CaptureN pk kr (Get '[HTML,JSON]
                        (Tagged opt (Tagged '(rep, (TableDef n rec pk uk fk)) (Maybe ar))))
    type PersAPI rep opt back (TableDef n rec pk uk fk) ar kr dr =
        PersListAPI rep opt back (TableDef n rec pk uk fk) ar kr dr
        :<|>
        PersRecAPI rep opt back (TableDef n rec pk uk fk) ar kr dr
        :<|>
        n :> (
            PersInsAPI (IsAutoPKb rep back kr) rep back
                        (TableDef n rec pk uk fk) ar kr dr
            :<|>
            ReqBody '[JSON] (PRecs rep rec ar) :> Put '[JSON] (PPks rep rec pk kr)
            :<|>
            ReqBody '[JSON] (PPks rep rec pk kr) :> Delete '[JSON] Int
            )
    persServer pr po pb pa =
        fmap (Tagged . Tagged) (sel pTab mempty)
        :<|>
        curryN pRep
            ( (fmap (Tagged . Tagged . listToMaybe) . sel pTab . Equal pPkN)
              :: kr -> PersMonad back
                        (Tagged opt
                            (Tagged '(rep, (TableDef n rec pk uk fk))
                                    (Maybe ar)
                            )
                        )
            )
        :<|>
        persInsServer (proxy# :: Proxy# (IsAutoPKb rep back kr)) pr pb pa
        :<|>
        fmap Tagged . upd pTab . untag
        :<|>
        fmap sum . mapM (del pTab . Equal pPkN) . untag
      where
        pRec    = Proxy :: Proxy '(rep, rec)
        pTab    = Proxy :: Proxy '(rep, TableDef n rec pk uk fk)
        pPkN    = Proxy :: Proxy pk
        pPk     = Proxy :: Proxy '(rep, ProjNames rec pk)
        pRep  = Proxy :: Proxy rep

-- local classes
type family CaptureN pks ks rest where
    CaptureN '[pk] (k,()) rest = Capture pk k :> rest
    CaptureN (p1 ': ps) (k1,k2) rest = Capture p1 k1 :> CaptureN ps k2 rest

--type family MkLinkN rep ks ep where
--    MkLinkN Plain (k,()) ep = k -> MkLink ep
--    MkLinkN Plain (k,ks) ep = k -> MkLinkN Plain ks ep

class   ( TableLike a
        , Rep rep (RecordDef a) ar
        , Rep rep (Key a) kr
        , Rep rep (DataRecord a) dr
        )
        => PersServantIns (isAuto::Bool) (rep::R) (back:: *) (a::DataDef *) ar kr dr
                | rep a -> ar, rep a -> kr, rep a -> dr where
    type PersInsAPI isAuto rep back a ar kr dr
    persInsServer :: Proxy# isAuto -> Proxy# rep -> Proxy# back -> Proxy '(a,ar,kr,dr)
        -> ServerT (PersInsAPI isAuto rep back a ar kr dr) (PersMonad back)

instance (DML rep back a ar kr dr, IsAutoPK rep back kr, TableLike a)
        => PersServantIns True rep back a ar kr dr
  where
    type PersInsAPI True rep back a ar kr dr
        = ReqBody '[JSON] (PRecs rep (DataRecord a) dr)
                :> Post '[JSON] (Tagged '(rep,Key a) [kr])
        -- :<|>
        -- "new" :> Get '[HTML] (Tagged opt (Tagged '(rep, (DataRecord a)) (Maybe dr)))
    persInsServer _ _ _ _
        =       fmap Tagged . insAuto pTab . untag
        -- :<|>
      where
        pTab = Proxy :: Proxy '(rep, a)

instance (DML rep back a ar kr dr, TableLike a)
        => PersServantIns False rep back a ar kr dr
  where
    type PersInsAPI False rep back a ar kr dr
        = ReqBody '[JSON] (PRecs rep (RecordDef a) ar)
        :> Post '[JSON] ()
    persInsServer _ _ _ _ = ins pTab . untag
      where
        pTab = Proxy :: Proxy '(rep, a)

--------- ServantDoc ---------------
-- Надо делать спец типы и их для разных БД адаптировать и для документации
instance ToSample () () where
    toSample _ = Nothing
instance ToSample Int64 Int64 where
    toSample _ = Just 11
instance ToSample Int Int where
    toSample _ = Just 1
instance ToSample [Int64] [Int64] where
    toSample _ = Just [11,21]
instance ToSample [(Int64,())] [(Int64,())] where
    toSample _ = Just [(11,()),(21,())]
instance ToSample Double Double where
    toSample _ = Just 1.2
instance ToSample [Double] [Double] where
    toSample _ = Just [1.2,2.2]
instance ToSample Text Text where
    toSample _ = Just "Some text"
instance ToSample [Text] [Text] where
    toSample _ = Just ["English", "Русский", "עברית", "日本の"]
instance ToSample ByteString ByteString where
    toSample _ = Just "Some long text"
instance ToSample [ByteString] [ByteString] where
    toSample _ = Just ["Some long text", "More and more..."]

instance ToSample (Tagged '(Plain, '(n,Int64)) Int64)
                  (Tagged '(Plain, '(n,Int64)) Int64)
  where
    toSample = fmap Tagged . toSample . fmap untag
instance ToSample (Tagged '(Plain, '(n,Double)) Double)
                  (Tagged '(Plain, '(n,Double)) Double)
  where
    toSample = fmap Tagged . toSample . fmap untag
instance ToSample (Tagged '(Plain, '(n,T.Text)) T.Text)
                  (Tagged '(Plain, '(n,T.Text)) T.Text)
  where
    toSample = fmap Tagged . toSample . fmap untag

instance ToSample t t =>
           ToSample (Tagged '(Plain, '(n,Maybe t)) t)
                    (Tagged '(Plain, '(n,Maybe t)) t)
  where
    toSample = fmap Tagged . toSample . fmap untag

instance (ToSample v v')
    => ToSample (Tagged '(Plain, '[ '(n,v)]) (v ,()))
                (Tagged '(Plain, '[ '(n,v)]) (v',()))
  where
    toSample = fmap (Tagged . (,())) . toSample . fmap (fst . untag)

instance (Rep Plain (x1 ': xs) rs
        , Rep Plain '[x] (r,())
        , ToSample (Tagged '(Plain, x) r) (Tagged '(Plain, x) r')
        , ToSample (Tagged '(Plain, x1 ': xs) rs) (Tagged '(Plain, x1 ': xs) rs')
        )
        => ToSample (Tagged '(Plain, ( x ': x1 ': xs)) (r ,rs ))
                    (Tagged '(Plain, ( x ': x1 ': xs)) (r',rs'))
  where
    toSample _  = fmap Tagged $ (,) <$> sv <*> sxs
      where
        sv  = fmap untag $ toSample (Proxy :: Proxy (Tagged '(Plain, x) r))
        sxs = fmap untag $ toSample (Proxy :: Proxy (Tagged '(Plain, x1 ': xs) rs))

instance (ToSample (Tagged '(Plain, xs) r) (Tagged '(Plain, xs) r')
        , Rep Plain xs r
        )
        => ToSample (Tagged '(Plain, xs) [r]) (Tagged '(Plain, xs) [r'])
  where
    toSample = fmap (fmap (:[])) . toSample . fmap (fmap head)

instance (ToSample (Tagged '(Plain,b,xs,ar,kr,dr) r)
                   (Tagged '(Plain,b,xs,ar,kr,dr) r')
        , Rep Plain xs r
        )
        => ToSample (Tagged '(Plain,b,xs,ar,kr,dr) [r])
                    (Tagged '(Plain,b,xs,ar,kr,dr) [r'])
  where
    toSample = fmap (fmap (:[])) . toSample . fmap (fmap head)

instance ToSample v v' => ToSample (Maybe v) (Maybe v') where
    toSamples _ =
         [ ("Found:", maybe Nothing Just $ toSample (Proxy :: Proxy v))
         , ("Not found:", Nothing)
         ]


instance ToSample (Tagged '(Plain, x) r) (Tagged '(Plain, x) r')
    =>   ToSample (Tagged '(Plain, (x :: [(Symbol, *)])) (Maybe r))
                  (Tagged '(Plain, x) (Maybe r'))
  where
    toSamples _ =
        [ ("Found:", maybe (Tagged Nothing) (retag . fmap Just)
                        $ toSample (Proxy :: Proxy (Tagged '(Plain, x) r)))
        , ("Not found:", Tagged Nothing)
        ]

instance ToSample (Tagged '(Plain, x) r) (Tagged '(Plain, x) r')
    =>   ToSample (Tagged '(Plain, (x :: (Symbol, *))) (Maybe r))
                  (Tagged '(Plain, x) (Maybe r'))
  where
    toSamples _ =
        [ ("Found:", maybe (Tagged Nothing) (retag . fmap Just)
                        $ toSample (Proxy :: Proxy (Tagged '(Plain, x) r)))
        , ("Not found:", Tagged Nothing)
        ]

instance ToSample (Tagged '(rep, r) v) (Tagged '(rep, r) v')
    => ToSample (Tagged '(rep, TableDef n r p u f) v)
                (Tagged '(rep, TableDef n r p u f) v')
  where
    toSample
        = fmap retag
        . (toSample :: Proxy (Tagged '(rep, r) v) -> Maybe (Tagged '(rep, r) v'))
        . fmap retag

instance ToSample (Tagged '(rep, r) v) (Tagged '(rep, r) v')
    => ToSample (Tagged '(rep,b,TableDef n r p u f,ar,kr,dr) v)
                (Tagged '(rep,b,TableDef n r p u f,ar,kr,dr) v')
  where
    toSample
        = fmap retag
        . (toSample :: Proxy (Tagged '(rep, r) v) -> Maybe (Tagged '(rep, r) v'))
        . fmap retag

----------------------------------------------------------------------

instance (FromText x) => FromText (Maybe x)
  where
    fromText = Just . fromText

instance (FromText x) => FromText (x,())
  where
    fromText = fmap (,()) . fromText

instance (FromText x, FromText (y,z)) => FromText (x,(y,z))
  where
    fromText = uncurry (liftA2 (,)) . bimap fromText fromText . T.break (=='_')

instance (ToText x) => ToText (Maybe x)
  where
    toText = maybe "" toText

instance (ToText x) => ToText (x, ())
  where
    toText = toText . fst
