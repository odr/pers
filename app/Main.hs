{-# LANGUAGE FlexibleContexts #-}
module Main where

import NamedBTree

main :: IO ()
main = print
    -- TF - FD
    -- test7 defined ---
    -- test7 -- 7s
    -- (test7 ::T7) -- 10s
    -- (f1_7 ()) -- 16s
    -- ((f1_7 $ ()) :: T7) -- 25s

    -- test11 defined ---
    -- test7 -- 12s
    -- test11 -- 13s
    -- (test4::T4) -- 11s - 3.5s
    -- (test5 :: T5) -- 12s
    -- (test6 :: T6) -- 12s
    -- (test7 :: T7) -- 14s - 7s
    -- (test8 ::T8) -- 28s, no memory lost - 8s
    (test11 ::T11) -- ? - 17s

type T1  = ()  <+ "1" :>Int
type T2  = T1  <+ "2" :>Int
type T3  = T2  <+ "3" :>Int
type T4  = T3  <+ "4" :>Int
type T5  = T4  <+ "5" :>Int
type T6  = T5  <+ "6" :>Int
type T7  = T6  <+ "7" :>Int
type T8  = T7  <+ "8" :>Int
type T9  = T8  <+ "9" :>Int
type T10 = T9  <+ "10":>Int
type T11 = T10 <+ "11":>Int
type T12 = T11 <+ "12":>Int
type T13 = T12 <+ "13":>Int
type T14 = T13 <+ "14":>Int
type T15 = T14 <+ "15":>Int
type T16 = T15 <+ "16":>Int
type T17 = T16 <+ "17":>Int
type T18 = T17 <+ "18":>Int
type T19 = T18 <+ "19":>Int
type T20 = T19 <+ "20":>Int
type T21 = T20 <+ "21":>Int
type T22 = T21 <+ "22":>Int
type T23 = T22 <+ "23":>Int
type T24 = T23 <+ "24":>Int

f1  a = a <+ (V  1 ::  "1":>Int)
f2  a = a <+ (V  2 ::  "2":>Int)
f3  a = a <+ (V  3 ::  "3":>Int)
f4  a = a <+ (V  4 ::  "4":>Int)
f5  a = a <+ (V  5 ::  "5":>Int)

f1_5 a = f1.f2.f3.f4.f5 $ a
f1_6 a = f1_5.f6 $ a
f1_7 a = f1_6.f7 $ a

f6  a = a <+ (V  6 ::  "6":>Int)
f7  a = a <+ (V  7 ::  "7":>Int)
f8  a = a <+ (V  8 ::  "8":>Int)
f9  a = a <+ (V  9 ::  "9":>Int)
f10 a = a <+ (V 10 :: "10":>Int)
f11 a = a <+ (V 11 :: "11":>Int)
f12 a = a <+ (V 12 :: "12":>Int)
f13 a = a <+ (V 13 :: "13":>Int)
f14 a = a <+ (V 14 :: "14":>Int)
f15 a = a <+ (V 15 :: "15":>Int)
f16 a = a <+ (V 16 :: "16":>Int)
f17 a = a <+ (V 17 :: "17":>Int)
f18 a = a <+ (V 18 :: "18":>Int)
f19 a = a <+ (V 19 :: "19":>Int)
f20 a = a <+ (V 20 :: "20":>Int)
f21 a = a <+ (V 21 :: "21":>Int)

f6_10  a = f6 . f7 . f8 . f9 . f10 $ a
f11_21 a = f11. f12. f13. f14. f15. f16. f17. f18. f19. f20. f21 $ a

test1 = ()       <+ (V 1 :: "1":>Int)   -- :: T1
test2  = test1   <+ (V 2 :: "2":>Int)   -- :: T2
test3  = test2   <+ (V 3 :: "3":>Int)   -- :: T3
test4  = test3   <+ (V 4 :: "4":>Int)   -- :: T4
test5  = test4   <+ (V 5 :: "5":>Int)   -- :: T5
test6  = test5   <+ (V 6 :: "6":>Int)   -- :: T6
test7  = test6   <+ (V 7 :: "7":>Int)   -- :: T7
test8  = test7   <+ (V 8 :: "8":>Int)   -- :: T8
test9  = test8   <+ (V 9 :: "9":>Int)   -- :: T9
test10 = test9   <+ (V 10 :: "10":>Int) -- :: T10
test11 = test10  <+ (V 11 :: "11":>Int) -- :: T11
{-
test12 = test11  <+ (V 12 :: "12":>Int) -- :: T12
test13 = test12  <+ (V 13 :: "13":>Int) -- :: T13
test14 = test13  <+ (V 14 :: "14":>Int) -- :: T14
test15 = test14  <+ (V 15 :: "15":>Int) -- :: T15
test16 = test15  <+ (V 16 :: "16":>Int) -- :: T16
test17 = test16  <+ (V 17 :: "17":>Int) -- :: T17
test18 = test17  <+ (V 18 :: "18":>Int) -- :: T18
test19 = test18  <+ (V 19 :: "19":>Int) -- :: T19
test20 = test19  <+ (V 20 :: "20":>Int) -- :: T20
test21 = test20  <+ (V 21 :: "21":>Int) -- :: T21
test22 = test21  <+ (V 22 :: "22":>Int) -- :: T22
-}
