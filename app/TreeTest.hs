{-# LANGUAGE FlexibleContexts #-}
module TreeTest where

import NamedBTree
import CommonTest
import TreeTypes

{-
tests running with TF (NamedBTree2) and FunDeps (NamedBTree).
I just run
> time stack build
with different strings uncommented.
Values "xxx --- yyy" mean time with TF and time with FD respectively

FD faster significantly here!!
-}
run = do
     print
        -- test1
                -- t1 defined: 9.1s     --- 9.3
                -- t2 defined: 9.3s     --- 9.5
                -- t4 defined: 9.9s     --- 9.6
                -- t5 defined: 10s      --- 9.9
                -- t8 defined: 15.8s    --- 10.4
                -- t16 defined: 87s     --- 13.4
        -- test8
                -- t8 defined: 19s      --- 13.1
        $ () <+ a1 <+a2<+a3<+a4      -- 10,4s
                                        -- 10,8
        -- $ () <+ a1 <+a2<+a3<+a4<+a5<+a6<+a7<+a8  -- 13,2s --- 13.5
        -- (() <+ a1 <+a2<+a3<+a4<+a5<+a6<+a7<+a8 :: T8)   -- 32.8 --- 16.4
        -- $ ()<+a1<+a2<+a3<+a4<+a5<+a6<+a7<+a8<+a9<+a10<+a11<+a12<+a13<+a14<+a15<+a16
                    -- 22,4s --- 20.5
        -- $ ()<+a1<+a2<+a3<+a4<+a5<+a6<+a7<+a8<+a9<+a10<+a11<+a12<+a13<+a14<+a15<+a16
        --      <+a17<+a18<+a19<+a20<+a21<+a22<+a23<+a24 -- 42.4S --- 31.8
        -- $ ()<+a1<+a2<+a3<+a4<+a5<+a6<+a7<+a8<+a9<+a10<+a11<+a12<+a13<+a14<+a15<+a16
        --     <+a17<+a18<+a19<+a20<+a21<+a22<+a23<+a24<+a15<+a16<+a17<+a28<+a29<+a30<+a31<+a32 -- 71.3s --- 44.5
        -- $ f a1 a2 a3 a4 . f a5 a6 a7 a8 $ () -- 18s --- 13.4
        -- $ f a1 a2 a3 a4 . f a5 a6 a7 a8 . f a9 a10 a11 a12 . f a13 a14 a15 a16 $ () -- 84s --- 21.1

f a b c d = (<+a) . (<+b) . (<+c) . (<+d)

{-
t1 = ()       <+ a1
t2  = t1   <+ a2
t3  = t2   <+ a3
t4  = t3   <+ a4
t5  = t4   <+ a5
t6  = t5   <+ a6
t7  = t6   <+ a7
t8  = t7   <+ a8
t9  = t8   <+ a9
t10 = t9   <+ a10
t11 = t10  <+ a11
t12 = t11  <+ a12
t13 = t12  <+ a13
t14 = t13  <+ a14
t15 = t14  <+ a15
t16 = t15  <+ a16
t17 = t16  <+ a17
t18 = t17  <+ a18
t19 = t18  <+ a19
t20 = t19  <+ a20
-}
