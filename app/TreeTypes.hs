{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module TreeTypes where

import NamedBTree
import CommonTest

type family T1  where T1  = ()  <+ A1
type family T2  where T2  = T1  <+ A2
type family T3  where T3  = T2  <+ A3
type family T4  where T4  = T3  <+ A4
type family T5  where T5  = T4  <+ A5
type family T6  where T6  = T5  <+ A6
type family T7  where T7  = T6  <+ A7
type family T8  where T8  = T7  <+ A8
type family T9  where T9  = T8  <+ A9
type family T10 where T10 = T9  <+ A10
type family T11 where T11 = T10 <+ A11
type family T12 where T12 = T11 <+ A12
type family T13 where T13 = T12 <+ A13
type family T14 where T14 = T13 <+ A14
type family T15 where T15 = T14 <+ A15
type family T16 where T16 = T15 <+ A16
type family T17 where T17 = T16 <+ A17
type family T18 where T18 = T17 <+ A18
type family T19 where T19 = T18 <+ A19
type family T20 where T20 = T19 <+ A20
type family T21 where T21 = T20 <+ A21
type family T22 where T22 = T21 <+ A22
type family T23 where T23 = T22 <+ A23
type family T24 where T24 = T23 <+ A24
