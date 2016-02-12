{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -O0 #-}
module NRTH where
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.TH
import Data.List
{-
-}

singletons [d|
    deleteBy'                :: (a -> b -> Bool) -> a -> [b] -> [b]
    deleteBy' _  _ []        = []
    deleteBy' eq x (y:ys)    = if x `eq` y then ys else y : deleteBy' eq x ys

    minusBy                  :: (a -> b -> Bool) -> [a] -> [b] -> [a]
    minusBy f                = foldl (flip (deleteBy' (flip f)))

    eqFst :: (Eq a) => (a,b) -> a -> Bool
    eqFst a b = fst a == b
  |]

