{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ListTest where

import List
import CommonTest

run = do
    print
    --    (l10 :: L10) -- 13s (with (+>) == (,) !!!) -- 44s
        -- l10             -- 13s (with (+>) == (,) !!!) -- 21s
        (l20::L20)  -- with TF definitions and (1..50) - 19s. With/without ::L20 the same result!!!
                    -- if l25 defined - 28s
                    -- if l30 defined - 44s
                    -- if l35 defined - 70s
                    -- if l40 defined - 104s
    print $ a25 +> l19 -- l49 defined (104s) - 119s

type L1 = A1 +> ()
type family L2 where L2 = A2 +> L1
type family L3 where L3 = A3 +> L2
type family L4 where L4 = A4 +> L3
type family L5 where L5 = A5 +> L4
type family L6 where L6 = A6 +> L5
type family L7 where L7 = A7 +> L6
type family L8 where L8 = A8 +> L7
type family L9 where L9 = A9 +> L8
type family L10 where L10 = A10 +> L9
type family L11 where L11 = A11 +> L10
type family L12 where L12 = A12 +> L11
type family L13 where L13 = A13 +> L12
type family L14 where L14 = A14 +> L13
type family L15 where L15 = A15 +> L14
type family L16 where L16 = A16 +> L15
type family L17 where L17 = A17 +> L16
type family L18 where L18 = A18 +> L17
type family L19 where L19 = A19 +> L18
type family L20 where L20 = A20 +> L19
type family L21 where L21 = A21 +> L20
type family L22 where L22 = A22 +> L21
type family L23 where L23 = A23 +> L22
type family L24 where L24 = A24 +> L23
type family L25 where L25 = A25 +> L24
type family L26 where L26 = A26 +> L25
type family L27 where L27 = A27 +> L26
type family L28 where L28 = A28 +> L27
type family L29 where L29 = A29 +> L28
type family L30 where L30 = A30 +> L29
type family L31 where L31 = A31 +> L30
type family L32 where L32 = A32 +> L31
type family L33 where L33 = A33 +> L32
type family L34 where L34 = A34 +> L33
type family L35 where L35 = A35 +> L34
type family L36 where L36 = A36 +> L35
type family L37 where L37 = A37 +> L36
type family L38 where L38 = A38 +> L37
type family L39 where L39 = A39 +> L38
type family L40 where L40 = A40 +> L39
type family L41 where L41 = A41 +> L40
type family L42 where L42 = A42 +> L41
type family L43 where L43 = A43 +> L42
type family L44 where L44 = A44 +> L43
type family L45 where L45 = A45 +> L44
type family L46 where L46 = A46 +> L45
type family L47 where L47 = A47 +> L46
type family L48 where L48 = A48 +> L47
type family L49 where L49 = A49 +> L48
type family L50 where L50 = A50 +> L49
{-
type L51 = A51 +> L50
type L52 = A52 +> L51
type L53 = A53 +> L52
type L54 = A54 +> L53
type L55 = A55 +> L54
type L56 = A56 +> L55
type L57 = A57 +> L56
type L58 = A58 +> L57
type L59 = A59 +> L58
type L60 = A60 +> L59
type L61 = A61 +> L60
type L62 = A62 +> L61
type L63 = A63 +> L62
type L64 = A64 +> L63
type L65 = A65 +> L64
type L66 = A66 +> L65
type L67 = A67 +> L66
type L68 = A68 +> L67
type L69 = A69 +> L68
type L70 = A70 +> L69
type L71 = A71 +> L70
type L72 = A72 +> L71
type L73 = A73 +> L72
type L74 = A74 +> L73
type L75 = A75 +> L74
type L76 = A76 +> L75
type L77 = A77 +> L76
type L78 = A78 +> L77
type L79 = A79 +> L78
type L80 = A80 +> L79
type L81 = A81 +> L80
type L82 = A82 +> L81
type L83 = A83 +> L82
type L84 = A84 +> L83
type L85 = A85 +> L84
type L86 = A86 +> L85
type L87 = A87 +> L86
type L88 = A88 +> L87
type L89 = A89 +> L88
type L90 = A90 +> L89
type L91 = A91 +> L90
type L92 = A92 +> L91
type L93 = A93 +> L92
type L94 = A94 +> L93
type L95 = A95 +> L94
type L96 = A96 +> L95
type L97 = A97 +> L96
type L98 = A98 +> L97
type L99 = A99 +> L98
-}

l1 = a1 +> () :: L1
l2 = a2 +> l1 :: L2
l3 = a3 +> l2
l4 = a4 +> l3
l5 = a5 +> l4
l6 = a6 +> l5
l7 = a7 +> l6
l8 = a8 +> l7
l9 = a9 +> l8
l10 = a10 +> l9
l11 = a11 +> l10
l12 = a12 +> l11
l13 = a13 +> l12
l14 = a14 +> l13
l15 = a15 +> l14
l16 = a16 +> l15
l17 = a17 +> l16
l18 = a18 +> l17
l19 = a19 +> l18
l20 = a20 +> l19
l21 = a21 +> l20
l22 = a22 +> l21
l23 = a23 +> l22
l24 = a24 +> l23
l25 = a25 +> l24
l26 = a26 +> l25
l27 = a27 +> l26
l28 = a28 +> l27
l29 = a29 +> l28
l30 = a30 +> l29
l31 = a31 +> l30
l32 = a32 +> l31
l33 = a33 +> l32
l34 = a34 +> l33
l35 = a35 +> l34
l36 = a36 +> l35
l37 = a37 +> l36
l38 = a38 +> l37
l39 = a39 +> l38
l40 = a40 +> l39
{-
l41 = a41 +> l40
l42 = a42 +> l41
l43 = a43 +> l42
l44 = a44 +> l43
l45 = a45 +> l44
l46 = a46 +> l45
l47 = a47 +> l46
l48 = a48 +> l47
l49 = a49 +> l48
l50 = a50 +> l49
l51 = a51 +> l50
l52 = a52 +> l51
l53 = a53 +> l52
l54 = a54 +> l53
l55 = a55 +> l54
l56 = a56 +> l55
l57 = a57 +> l56
l58 = a58 +> l57
l59 = a59 +> l58
l60 = a60 +> l59
l61 = a61 +> l60
l62 = a62 +> l61

l63 = a63 +> l62
l64 = a64 +> l63
l65 = a65 +> l64
l66 = a66 +> l65
l67 = a67 +> l66
l68 = a68 +> l67
l69 = a69 +> l68
l70 = a70 +> l69
l71 = a71 +> l70
l72 = a72 +> l71
l73 = a73 +> l72
l74 = a74 +> l73
l75 = a75 +> l74
l76 = a76 +> l75
l77 = a77 +> l76
l78 = a78 +> l77
l79 = a79 +> l78
l80 = a80 +> l79
l81 = a81 +> l80
l82 = a82 +> l81
l83 = a83 +> l82
l84 = a84 +> l83
l85 = a85 +> l84
l86 = a86 +> l85
l87 = a87 +> l86
l88 = a88 +> l87
l89 = a89 +> l88
l90 = a90 +> l89
l91 = a91 +> l90
l92 = a92 +> l91
l93 = a93 +> l92
l94 = a94 +> l93
l95 = a95 +> l94
l96 = a96 +> l95
l97 = a97 +> l96
l98 = a98 +> l97
l99 = a99 +> l98
-}
