{-# LANGUAGE FlexibleContexts #-}
module Main where

import NamedBTree2

main :: IO ()
main = print ((f1_7 $ ()) :: T7)

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
