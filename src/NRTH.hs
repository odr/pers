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

    minus                    :: (Eq a) => [a] -> [a] -> [a]
    minus                    = minusBy (==)

    eqFst :: (Eq a) => (a,b) -> a -> Bool
    eqFst a b = fst a == b

    projBy             :: (a -> b -> Bool) -> [a] -> [b] -> [a]
    projBy _  []       []       =  []
    projBy _  []       (_:_)    =  []
    projBy _  (_:_)    []       =  []
    projBy f xs@(_:_) ys@(_:_) =  filter (\x -> any_ (f x) ys) xs

  |]

