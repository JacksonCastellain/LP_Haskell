{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,344) ([6016,2088,16422,0,0,0,0,61440,58875,1217,0,0,0,0,0,0,0,12032,4176,32844,10263,9736,3008,1044,19,0,2048,16,0,0,0,512,0,0,0,0,64,20527,19472,0,8192,49152,47087,4871,63456,50127,9,0,0,0,0,48128,16704,304,41054,38944,12032,4176,32844,10263,9736,3008,1044,57363,2565,2434,752,49413,30724,33409,608,16572,12353,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,8,0,0,0,0,0,32768,10263,9736,3008,1044,19,520,48,0,0,30720,33409,608,32508,28793,1,16384,0,0,0,8192,49160,0,0,0,0,0,61440,62971,1217,65016,28914,2,128,0,0,0,12032,4176,32844,10263,9736,0,34816,0,1024,0,1776,55557,30724,33409,608,0,0,0,0,0,0,0,14208,51240,38,0,0,0,0,0,260,24,33280,3072,0,0,0,0,0,0,8192,2,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","Type","ExpList","TypeList","num","true","false","string","\"++\"","\"length\"","\"String\"","'+'","'-'","'*'","\"&&\"","\"||\"","'('","')'","\"if\"","\"then\"","\"else\"","'>'","\"==\"","'<'","'\\\\'","\"->\"","':'","\"Int\"","\"Bool\"","','","\"proj\"","\"let\"","\"in\"","'='","varid","%eof"]
        bit_start = st Prelude.* 39
        bit_end = (st Prelude.+ 1) Prelude.* 39
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..38]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_2
action_0 (9) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (13) = happyShift action_7
action_0 (20) = happyShift action_8
action_0 (22) = happyShift action_9
action_0 (28) = happyShift action_10
action_0 (34) = happyShift action_11
action_0 (35) = happyShift action_12
action_0 (38) = happyShift action_13
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (8) = happyShift action_2
action_3 (9) = happyShift action_4
action_3 (10) = happyShift action_5
action_3 (11) = happyShift action_6
action_3 (12) = happyShift action_21
action_3 (13) = happyShift action_7
action_3 (15) = happyShift action_22
action_3 (16) = happyShift action_23
action_3 (17) = happyShift action_24
action_3 (18) = happyShift action_25
action_3 (19) = happyShift action_26
action_3 (20) = happyShift action_8
action_3 (22) = happyShift action_9
action_3 (25) = happyShift action_27
action_3 (26) = happyShift action_28
action_3 (27) = happyShift action_29
action_3 (28) = happyShift action_10
action_3 (34) = happyShift action_11
action_3 (35) = happyShift action_12
action_3 (38) = happyShift action_13
action_3 (39) = happyAccept
action_3 (4) = happyGoto action_20
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 _ = happyReduce_4

action_7 (8) = happyShift action_2
action_7 (9) = happyShift action_4
action_7 (10) = happyShift action_5
action_7 (11) = happyShift action_6
action_7 (13) = happyShift action_7
action_7 (20) = happyShift action_8
action_7 (22) = happyShift action_9
action_7 (28) = happyShift action_10
action_7 (34) = happyShift action_11
action_7 (35) = happyShift action_12
action_7 (38) = happyShift action_13
action_7 (4) = happyGoto action_19
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (8) = happyShift action_2
action_8 (9) = happyShift action_4
action_8 (10) = happyShift action_5
action_8 (11) = happyShift action_6
action_8 (13) = happyShift action_7
action_8 (20) = happyShift action_8
action_8 (22) = happyShift action_9
action_8 (28) = happyShift action_10
action_8 (34) = happyShift action_11
action_8 (35) = happyShift action_12
action_8 (38) = happyShift action_13
action_8 (4) = happyGoto action_18
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (8) = happyShift action_2
action_9 (9) = happyShift action_4
action_9 (10) = happyShift action_5
action_9 (11) = happyShift action_6
action_9 (13) = happyShift action_7
action_9 (20) = happyShift action_8
action_9 (22) = happyShift action_9
action_9 (28) = happyShift action_10
action_9 (34) = happyShift action_11
action_9 (35) = happyShift action_12
action_9 (38) = happyShift action_13
action_9 (4) = happyGoto action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (38) = happyShift action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (8) = happyShift action_15
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (38) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_5

action_14 (37) = happyShift action_44
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (8) = happyShift action_2
action_15 (9) = happyShift action_4
action_15 (10) = happyShift action_5
action_15 (11) = happyShift action_6
action_15 (13) = happyShift action_7
action_15 (20) = happyShift action_8
action_15 (22) = happyShift action_9
action_15 (28) = happyShift action_10
action_15 (34) = happyShift action_11
action_15 (35) = happyShift action_12
action_15 (38) = happyShift action_13
action_15 (4) = happyGoto action_43
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (30) = happyShift action_42
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_2
action_17 (9) = happyShift action_4
action_17 (10) = happyShift action_5
action_17 (11) = happyShift action_6
action_17 (12) = happyShift action_21
action_17 (13) = happyShift action_7
action_17 (15) = happyShift action_22
action_17 (16) = happyShift action_23
action_17 (17) = happyShift action_24
action_17 (18) = happyShift action_25
action_17 (19) = happyShift action_26
action_17 (20) = happyShift action_8
action_17 (22) = happyShift action_9
action_17 (23) = happyShift action_41
action_17 (25) = happyShift action_27
action_17 (26) = happyShift action_28
action_17 (27) = happyShift action_29
action_17 (28) = happyShift action_10
action_17 (34) = happyShift action_11
action_17 (35) = happyShift action_12
action_17 (38) = happyShift action_13
action_17 (4) = happyGoto action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (8) = happyShift action_2
action_18 (9) = happyShift action_4
action_18 (10) = happyShift action_5
action_18 (11) = happyShift action_6
action_18 (12) = happyShift action_21
action_18 (13) = happyShift action_7
action_18 (15) = happyShift action_22
action_18 (16) = happyShift action_23
action_18 (17) = happyShift action_24
action_18 (18) = happyShift action_25
action_18 (19) = happyShift action_26
action_18 (20) = happyShift action_8
action_18 (21) = happyShift action_39
action_18 (22) = happyShift action_9
action_18 (25) = happyShift action_27
action_18 (26) = happyShift action_28
action_18 (27) = happyShift action_29
action_18 (28) = happyShift action_10
action_18 (33) = happyShift action_40
action_18 (34) = happyShift action_11
action_18 (35) = happyShift action_12
action_18 (38) = happyShift action_13
action_18 (4) = happyGoto action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (8) = happyShift action_2
action_19 (9) = happyShift action_4
action_19 (10) = happyShift action_5
action_19 (11) = happyShift action_6
action_19 (12) = happyShift action_21
action_19 (13) = happyShift action_7
action_19 (15) = happyShift action_22
action_19 (16) = happyShift action_23
action_19 (17) = happyShift action_24
action_19 (18) = happyShift action_25
action_19 (19) = happyShift action_26
action_19 (20) = happyShift action_8
action_19 (22) = happyShift action_9
action_19 (25) = happyShift action_27
action_19 (26) = happyShift action_28
action_19 (27) = happyShift action_29
action_19 (28) = happyShift action_10
action_19 (34) = happyShift action_11
action_19 (35) = happyShift action_12
action_19 (38) = happyShift action_13
action_19 (4) = happyGoto action_20
action_19 _ = happyReduce_8

action_20 (8) = happyShift action_2
action_20 (9) = happyShift action_4
action_20 (10) = happyShift action_5
action_20 (11) = happyShift action_6
action_20 (12) = happyShift action_21
action_20 (13) = happyShift action_7
action_20 (15) = happyShift action_22
action_20 (16) = happyShift action_23
action_20 (17) = happyShift action_24
action_20 (18) = happyShift action_25
action_20 (19) = happyShift action_26
action_20 (20) = happyShift action_8
action_20 (22) = happyShift action_9
action_20 (25) = happyShift action_27
action_20 (26) = happyShift action_28
action_20 (27) = happyShift action_29
action_20 (28) = happyShift action_10
action_20 (34) = happyShift action_11
action_20 (35) = happyShift action_12
action_20 (38) = happyShift action_13
action_20 (4) = happyGoto action_20
action_20 _ = happyReduce_21

action_21 (8) = happyShift action_2
action_21 (9) = happyShift action_4
action_21 (10) = happyShift action_5
action_21 (11) = happyShift action_6
action_21 (13) = happyShift action_7
action_21 (20) = happyShift action_8
action_21 (22) = happyShift action_9
action_21 (28) = happyShift action_10
action_21 (34) = happyShift action_11
action_21 (35) = happyShift action_12
action_21 (38) = happyShift action_13
action_21 (4) = happyGoto action_38
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (8) = happyShift action_2
action_22 (9) = happyShift action_4
action_22 (10) = happyShift action_5
action_22 (11) = happyShift action_6
action_22 (13) = happyShift action_7
action_22 (20) = happyShift action_8
action_22 (22) = happyShift action_9
action_22 (28) = happyShift action_10
action_22 (34) = happyShift action_11
action_22 (35) = happyShift action_12
action_22 (38) = happyShift action_13
action_22 (4) = happyGoto action_37
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (8) = happyShift action_2
action_23 (9) = happyShift action_4
action_23 (10) = happyShift action_5
action_23 (11) = happyShift action_6
action_23 (13) = happyShift action_7
action_23 (20) = happyShift action_8
action_23 (22) = happyShift action_9
action_23 (28) = happyShift action_10
action_23 (34) = happyShift action_11
action_23 (35) = happyShift action_12
action_23 (38) = happyShift action_13
action_23 (4) = happyGoto action_36
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (8) = happyShift action_2
action_24 (9) = happyShift action_4
action_24 (10) = happyShift action_5
action_24 (11) = happyShift action_6
action_24 (13) = happyShift action_7
action_24 (20) = happyShift action_8
action_24 (22) = happyShift action_9
action_24 (28) = happyShift action_10
action_24 (34) = happyShift action_11
action_24 (35) = happyShift action_12
action_24 (38) = happyShift action_13
action_24 (4) = happyGoto action_35
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (8) = happyShift action_2
action_25 (9) = happyShift action_4
action_25 (10) = happyShift action_5
action_25 (11) = happyShift action_6
action_25 (13) = happyShift action_7
action_25 (20) = happyShift action_8
action_25 (22) = happyShift action_9
action_25 (28) = happyShift action_10
action_25 (34) = happyShift action_11
action_25 (35) = happyShift action_12
action_25 (38) = happyShift action_13
action_25 (4) = happyGoto action_34
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (8) = happyShift action_2
action_26 (9) = happyShift action_4
action_26 (10) = happyShift action_5
action_26 (11) = happyShift action_6
action_26 (13) = happyShift action_7
action_26 (20) = happyShift action_8
action_26 (22) = happyShift action_9
action_26 (28) = happyShift action_10
action_26 (34) = happyShift action_11
action_26 (35) = happyShift action_12
action_26 (38) = happyShift action_13
action_26 (4) = happyGoto action_33
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (8) = happyShift action_2
action_27 (9) = happyShift action_4
action_27 (10) = happyShift action_5
action_27 (11) = happyShift action_6
action_27 (13) = happyShift action_7
action_27 (20) = happyShift action_8
action_27 (22) = happyShift action_9
action_27 (28) = happyShift action_10
action_27 (34) = happyShift action_11
action_27 (35) = happyShift action_12
action_27 (38) = happyShift action_13
action_27 (4) = happyGoto action_32
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (8) = happyShift action_2
action_28 (9) = happyShift action_4
action_28 (10) = happyShift action_5
action_28 (11) = happyShift action_6
action_28 (13) = happyShift action_7
action_28 (20) = happyShift action_8
action_28 (22) = happyShift action_9
action_28 (28) = happyShift action_10
action_28 (34) = happyShift action_11
action_28 (35) = happyShift action_12
action_28 (38) = happyShift action_13
action_28 (4) = happyGoto action_31
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (8) = happyShift action_2
action_29 (9) = happyShift action_4
action_29 (10) = happyShift action_5
action_29 (11) = happyShift action_6
action_29 (13) = happyShift action_7
action_29 (20) = happyShift action_8
action_29 (22) = happyShift action_9
action_29 (28) = happyShift action_10
action_29 (34) = happyShift action_11
action_29 (35) = happyShift action_12
action_29 (38) = happyShift action_13
action_29 (4) = happyGoto action_30
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (8) = happyShift action_2
action_30 (9) = happyShift action_4
action_30 (10) = happyShift action_5
action_30 (11) = happyShift action_6
action_30 (12) = happyShift action_21
action_30 (13) = happyShift action_7
action_30 (15) = happyShift action_22
action_30 (16) = happyShift action_23
action_30 (17) = happyShift action_24
action_30 (18) = happyShift action_25
action_30 (19) = happyShift action_26
action_30 (20) = happyShift action_8
action_30 (22) = happyShift action_9
action_30 (25) = happyShift action_27
action_30 (26) = happyShift action_28
action_30 (27) = happyShift action_29
action_30 (28) = happyShift action_10
action_30 (34) = happyShift action_11
action_30 (35) = happyShift action_12
action_30 (38) = happyShift action_13
action_30 (4) = happyGoto action_20
action_30 _ = happyReduce_14

action_31 (8) = happyShift action_2
action_31 (9) = happyShift action_4
action_31 (10) = happyShift action_5
action_31 (11) = happyShift action_6
action_31 (12) = happyShift action_21
action_31 (13) = happyShift action_7
action_31 (15) = happyShift action_22
action_31 (16) = happyShift action_23
action_31 (17) = happyShift action_24
action_31 (18) = happyShift action_25
action_31 (19) = happyShift action_26
action_31 (20) = happyShift action_8
action_31 (22) = happyShift action_9
action_31 (25) = happyShift action_27
action_31 (26) = happyShift action_28
action_31 (27) = happyShift action_29
action_31 (28) = happyShift action_10
action_31 (34) = happyShift action_11
action_31 (35) = happyShift action_12
action_31 (38) = happyShift action_13
action_31 (4) = happyGoto action_20
action_31 _ = happyReduce_13

action_32 (8) = happyShift action_2
action_32 (9) = happyShift action_4
action_32 (10) = happyShift action_5
action_32 (11) = happyShift action_6
action_32 (12) = happyShift action_21
action_32 (13) = happyShift action_7
action_32 (15) = happyShift action_22
action_32 (16) = happyShift action_23
action_32 (17) = happyShift action_24
action_32 (18) = happyShift action_25
action_32 (19) = happyShift action_26
action_32 (20) = happyShift action_8
action_32 (22) = happyShift action_9
action_32 (25) = happyShift action_27
action_32 (26) = happyShift action_28
action_32 (27) = happyShift action_29
action_32 (28) = happyShift action_10
action_32 (34) = happyShift action_11
action_32 (35) = happyShift action_12
action_32 (38) = happyShift action_13
action_32 (4) = happyGoto action_20
action_32 _ = happyReduce_12

action_33 (8) = happyShift action_2
action_33 (9) = happyShift action_4
action_33 (10) = happyShift action_5
action_33 (11) = happyShift action_6
action_33 (12) = happyShift action_21
action_33 (13) = happyShift action_7
action_33 (15) = happyShift action_22
action_33 (16) = happyShift action_23
action_33 (17) = happyShift action_24
action_33 (18) = happyShift action_25
action_33 (19) = happyShift action_26
action_33 (20) = happyShift action_8
action_33 (22) = happyShift action_9
action_33 (25) = happyShift action_27
action_33 (26) = happyShift action_28
action_33 (27) = happyShift action_29
action_33 (28) = happyShift action_10
action_33 (34) = happyShift action_11
action_33 (35) = happyShift action_12
action_33 (38) = happyShift action_13
action_33 (4) = happyGoto action_20
action_33 _ = happyReduce_16

action_34 (8) = happyShift action_2
action_34 (9) = happyShift action_4
action_34 (10) = happyShift action_5
action_34 (11) = happyShift action_6
action_34 (12) = happyShift action_21
action_34 (13) = happyShift action_7
action_34 (15) = happyShift action_22
action_34 (16) = happyShift action_23
action_34 (17) = happyShift action_24
action_34 (18) = happyShift action_25
action_34 (19) = happyShift action_26
action_34 (20) = happyShift action_8
action_34 (22) = happyShift action_9
action_34 (25) = happyShift action_27
action_34 (26) = happyShift action_28
action_34 (27) = happyShift action_29
action_34 (28) = happyShift action_10
action_34 (34) = happyShift action_11
action_34 (35) = happyShift action_12
action_34 (38) = happyShift action_13
action_34 (4) = happyGoto action_20
action_34 _ = happyReduce_15

action_35 (8) = happyShift action_2
action_35 (9) = happyShift action_4
action_35 (10) = happyShift action_5
action_35 (11) = happyShift action_6
action_35 (12) = happyShift action_21
action_35 (13) = happyShift action_7
action_35 (18) = happyShift action_25
action_35 (19) = happyShift action_26
action_35 (20) = happyShift action_8
action_35 (22) = happyShift action_9
action_35 (25) = happyShift action_27
action_35 (26) = happyShift action_28
action_35 (27) = happyShift action_29
action_35 (28) = happyShift action_10
action_35 (34) = happyShift action_11
action_35 (35) = happyShift action_12
action_35 (38) = happyShift action_13
action_35 (4) = happyGoto action_20
action_35 _ = happyReduce_11

action_36 (8) = happyShift action_2
action_36 (9) = happyShift action_4
action_36 (10) = happyShift action_5
action_36 (11) = happyShift action_6
action_36 (12) = happyShift action_21
action_36 (13) = happyShift action_7
action_36 (17) = happyShift action_24
action_36 (18) = happyShift action_25
action_36 (19) = happyShift action_26
action_36 (20) = happyShift action_8
action_36 (22) = happyShift action_9
action_36 (25) = happyShift action_27
action_36 (26) = happyShift action_28
action_36 (27) = happyShift action_29
action_36 (28) = happyShift action_10
action_36 (34) = happyShift action_11
action_36 (35) = happyShift action_12
action_36 (38) = happyShift action_13
action_36 (4) = happyGoto action_20
action_36 _ = happyReduce_10

action_37 (8) = happyShift action_2
action_37 (9) = happyShift action_4
action_37 (10) = happyShift action_5
action_37 (11) = happyShift action_6
action_37 (12) = happyShift action_21
action_37 (13) = happyShift action_7
action_37 (17) = happyShift action_24
action_37 (18) = happyShift action_25
action_37 (19) = happyShift action_26
action_37 (20) = happyShift action_8
action_37 (22) = happyShift action_9
action_37 (25) = happyShift action_27
action_37 (26) = happyShift action_28
action_37 (27) = happyShift action_29
action_37 (28) = happyShift action_10
action_37 (34) = happyShift action_11
action_37 (35) = happyShift action_12
action_37 (38) = happyShift action_13
action_37 (4) = happyGoto action_20
action_37 _ = happyReduce_9

action_38 (8) = happyShift action_2
action_38 (9) = happyShift action_4
action_38 (10) = happyShift action_5
action_38 (11) = happyShift action_6
action_38 (12) = happyShift action_21
action_38 (13) = happyShift action_7
action_38 (15) = happyShift action_22
action_38 (16) = happyShift action_23
action_38 (17) = happyShift action_24
action_38 (18) = happyShift action_25
action_38 (19) = happyShift action_26
action_38 (20) = happyShift action_8
action_38 (22) = happyShift action_9
action_38 (25) = happyShift action_27
action_38 (26) = happyShift action_28
action_38 (27) = happyShift action_29
action_38 (28) = happyShift action_10
action_38 (34) = happyShift action_11
action_38 (35) = happyShift action_12
action_38 (38) = happyShift action_13
action_38 (4) = happyGoto action_20
action_38 _ = happyReduce_7

action_39 _ = happyReduce_18

action_40 (8) = happyShift action_2
action_40 (9) = happyShift action_4
action_40 (10) = happyShift action_5
action_40 (11) = happyShift action_6
action_40 (13) = happyShift action_7
action_40 (20) = happyShift action_8
action_40 (22) = happyShift action_9
action_40 (28) = happyShift action_10
action_40 (34) = happyShift action_11
action_40 (35) = happyShift action_12
action_40 (38) = happyShift action_13
action_40 (4) = happyGoto action_52
action_40 (6) = happyGoto action_53
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (8) = happyShift action_2
action_41 (9) = happyShift action_4
action_41 (10) = happyShift action_5
action_41 (11) = happyShift action_6
action_41 (13) = happyShift action_7
action_41 (20) = happyShift action_8
action_41 (22) = happyShift action_9
action_41 (28) = happyShift action_10
action_41 (34) = happyShift action_11
action_41 (35) = happyShift action_12
action_41 (38) = happyShift action_13
action_41 (4) = happyGoto action_51
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (14) = happyShift action_47
action_42 (20) = happyShift action_48
action_42 (31) = happyShift action_49
action_42 (32) = happyShift action_50
action_42 (5) = happyGoto action_46
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (8) = happyShift action_2
action_43 (9) = happyShift action_4
action_43 (10) = happyShift action_5
action_43 (11) = happyShift action_6
action_43 (12) = happyShift action_21
action_43 (13) = happyShift action_7
action_43 (15) = happyShift action_22
action_43 (16) = happyShift action_23
action_43 (17) = happyShift action_24
action_43 (18) = happyShift action_25
action_43 (19) = happyShift action_26
action_43 (20) = happyShift action_8
action_43 (22) = happyShift action_9
action_43 (25) = happyShift action_27
action_43 (26) = happyShift action_28
action_43 (27) = happyShift action_29
action_43 (28) = happyShift action_10
action_43 (34) = happyShift action_11
action_43 (35) = happyShift action_12
action_43 (38) = happyShift action_13
action_43 (4) = happyGoto action_20
action_43 _ = happyReduce_22

action_44 (8) = happyShift action_2
action_44 (9) = happyShift action_4
action_44 (10) = happyShift action_5
action_44 (11) = happyShift action_6
action_44 (13) = happyShift action_7
action_44 (20) = happyShift action_8
action_44 (22) = happyShift action_9
action_44 (28) = happyShift action_10
action_44 (34) = happyShift action_11
action_44 (35) = happyShift action_12
action_44 (38) = happyShift action_13
action_44 (4) = happyGoto action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (8) = happyShift action_2
action_45 (9) = happyShift action_4
action_45 (10) = happyShift action_5
action_45 (11) = happyShift action_6
action_45 (12) = happyShift action_21
action_45 (13) = happyShift action_7
action_45 (15) = happyShift action_22
action_45 (16) = happyShift action_23
action_45 (17) = happyShift action_24
action_45 (18) = happyShift action_25
action_45 (19) = happyShift action_26
action_45 (20) = happyShift action_8
action_45 (22) = happyShift action_9
action_45 (25) = happyShift action_27
action_45 (26) = happyShift action_28
action_45 (27) = happyShift action_29
action_45 (28) = happyShift action_10
action_45 (34) = happyShift action_11
action_45 (35) = happyShift action_12
action_45 (36) = happyShift action_60
action_45 (38) = happyShift action_13
action_45 (4) = happyGoto action_20
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (29) = happyShift action_59
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_25

action_48 (14) = happyShift action_47
action_48 (20) = happyShift action_48
action_48 (31) = happyShift action_49
action_48 (32) = happyShift action_50
action_48 (5) = happyGoto action_57
action_48 (7) = happyGoto action_58
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_23

action_50 _ = happyReduce_24

action_51 (8) = happyShift action_2
action_51 (9) = happyShift action_4
action_51 (10) = happyShift action_5
action_51 (11) = happyShift action_6
action_51 (12) = happyShift action_21
action_51 (13) = happyShift action_7
action_51 (15) = happyShift action_22
action_51 (16) = happyShift action_23
action_51 (17) = happyShift action_24
action_51 (18) = happyShift action_25
action_51 (19) = happyShift action_26
action_51 (20) = happyShift action_8
action_51 (22) = happyShift action_9
action_51 (24) = happyShift action_56
action_51 (25) = happyShift action_27
action_51 (26) = happyShift action_28
action_51 (27) = happyShift action_29
action_51 (28) = happyShift action_10
action_51 (34) = happyShift action_11
action_51 (35) = happyShift action_12
action_51 (38) = happyShift action_13
action_51 (4) = happyGoto action_20
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (8) = happyShift action_2
action_52 (9) = happyShift action_4
action_52 (10) = happyShift action_5
action_52 (11) = happyShift action_6
action_52 (12) = happyShift action_21
action_52 (13) = happyShift action_7
action_52 (15) = happyShift action_22
action_52 (16) = happyShift action_23
action_52 (17) = happyShift action_24
action_52 (18) = happyShift action_25
action_52 (19) = happyShift action_26
action_52 (20) = happyShift action_8
action_52 (22) = happyShift action_9
action_52 (25) = happyShift action_27
action_52 (26) = happyShift action_28
action_52 (27) = happyShift action_29
action_52 (28) = happyShift action_10
action_52 (33) = happyShift action_55
action_52 (34) = happyShift action_11
action_52 (35) = happyShift action_12
action_52 (38) = happyShift action_13
action_52 (4) = happyGoto action_20
action_52 _ = happyReduce_29

action_53 (21) = happyShift action_54
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_17

action_55 (8) = happyShift action_2
action_55 (9) = happyShift action_4
action_55 (10) = happyShift action_5
action_55 (11) = happyShift action_6
action_55 (13) = happyShift action_7
action_55 (20) = happyShift action_8
action_55 (22) = happyShift action_9
action_55 (28) = happyShift action_10
action_55 (34) = happyShift action_11
action_55 (35) = happyShift action_12
action_55 (38) = happyShift action_13
action_55 (4) = happyGoto action_52
action_55 (6) = happyGoto action_70
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (8) = happyShift action_2
action_56 (9) = happyShift action_4
action_56 (10) = happyShift action_5
action_56 (11) = happyShift action_6
action_56 (13) = happyShift action_7
action_56 (20) = happyShift action_8
action_56 (22) = happyShift action_9
action_56 (28) = happyShift action_10
action_56 (34) = happyShift action_11
action_56 (35) = happyShift action_12
action_56 (38) = happyShift action_13
action_56 (4) = happyGoto action_69
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (21) = happyShift action_66
action_57 (29) = happyShift action_67
action_57 (33) = happyShift action_68
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (21) = happyShift action_65
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (8) = happyShift action_2
action_59 (9) = happyShift action_4
action_59 (10) = happyShift action_5
action_59 (11) = happyShift action_6
action_59 (13) = happyShift action_7
action_59 (14) = happyShift action_47
action_59 (20) = happyShift action_64
action_59 (22) = happyShift action_9
action_59 (28) = happyShift action_10
action_59 (31) = happyShift action_49
action_59 (32) = happyShift action_50
action_59 (34) = happyShift action_11
action_59 (35) = happyShift action_12
action_59 (38) = happyShift action_13
action_59 (4) = happyGoto action_62
action_59 (5) = happyGoto action_63
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (8) = happyShift action_2
action_60 (9) = happyShift action_4
action_60 (10) = happyShift action_5
action_60 (11) = happyShift action_6
action_60 (13) = happyShift action_7
action_60 (20) = happyShift action_8
action_60 (22) = happyShift action_9
action_60 (28) = happyShift action_10
action_60 (34) = happyShift action_11
action_60 (35) = happyShift action_12
action_60 (38) = happyShift action_13
action_60 (4) = happyGoto action_61
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (8) = happyShift action_2
action_61 (9) = happyShift action_4
action_61 (10) = happyShift action_5
action_61 (11) = happyShift action_6
action_61 (12) = happyShift action_21
action_61 (13) = happyShift action_7
action_61 (15) = happyShift action_22
action_61 (16) = happyShift action_23
action_61 (17) = happyShift action_24
action_61 (18) = happyShift action_25
action_61 (19) = happyShift action_26
action_61 (20) = happyShift action_8
action_61 (22) = happyShift action_9
action_61 (25) = happyShift action_27
action_61 (26) = happyShift action_28
action_61 (27) = happyShift action_29
action_61 (28) = happyShift action_10
action_61 (34) = happyShift action_11
action_61 (35) = happyShift action_12
action_61 (38) = happyShift action_13
action_61 (4) = happyGoto action_20
action_61 _ = happyReduce_6

action_62 (8) = happyShift action_2
action_62 (9) = happyShift action_4
action_62 (10) = happyShift action_5
action_62 (11) = happyShift action_6
action_62 (12) = happyShift action_21
action_62 (13) = happyShift action_7
action_62 (15) = happyShift action_22
action_62 (16) = happyShift action_23
action_62 (17) = happyShift action_24
action_62 (18) = happyShift action_25
action_62 (19) = happyShift action_26
action_62 (20) = happyShift action_8
action_62 (22) = happyShift action_9
action_62 (25) = happyShift action_27
action_62 (26) = happyShift action_28
action_62 (27) = happyShift action_29
action_62 (28) = happyShift action_10
action_62 (34) = happyShift action_11
action_62 (35) = happyShift action_12
action_62 (38) = happyShift action_13
action_62 (4) = happyGoto action_20
action_62 _ = happyReduce_20

action_63 (29) = happyShift action_67
action_63 _ = happyReduce_27

action_64 (8) = happyShift action_2
action_64 (9) = happyShift action_4
action_64 (10) = happyShift action_5
action_64 (11) = happyShift action_6
action_64 (13) = happyShift action_7
action_64 (14) = happyShift action_47
action_64 (20) = happyShift action_64
action_64 (22) = happyShift action_9
action_64 (28) = happyShift action_10
action_64 (31) = happyShift action_49
action_64 (32) = happyShift action_50
action_64 (34) = happyShift action_11
action_64 (35) = happyShift action_12
action_64 (38) = happyShift action_13
action_64 (4) = happyGoto action_18
action_64 (5) = happyGoto action_57
action_64 (7) = happyGoto action_58
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_26

action_66 _ = happyReduce_28

action_67 (14) = happyShift action_47
action_67 (20) = happyShift action_48
action_67 (31) = happyShift action_49
action_67 (32) = happyShift action_50
action_67 (5) = happyGoto action_63
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (14) = happyShift action_47
action_68 (20) = happyShift action_48
action_68 (31) = happyShift action_49
action_68 (32) = happyShift action_50
action_68 (5) = happyGoto action_71
action_68 (7) = happyGoto action_72
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (8) = happyShift action_2
action_69 (9) = happyShift action_4
action_69 (10) = happyShift action_5
action_69 (11) = happyShift action_6
action_69 (12) = happyShift action_21
action_69 (13) = happyShift action_7
action_69 (15) = happyShift action_22
action_69 (16) = happyShift action_23
action_69 (17) = happyShift action_24
action_69 (18) = happyShift action_25
action_69 (19) = happyShift action_26
action_69 (20) = happyShift action_8
action_69 (22) = happyShift action_9
action_69 (25) = happyShift action_27
action_69 (26) = happyShift action_28
action_69 (27) = happyShift action_29
action_69 (28) = happyShift action_10
action_69 (34) = happyShift action_11
action_69 (35) = happyShift action_12
action_69 (38) = happyShift action_13
action_69 (4) = happyGoto action_20
action_69 _ = happyReduce_19

action_70 _ = happyReduce_30

action_71 (29) = happyShift action_67
action_71 (33) = happyShift action_68
action_71 _ = happyReduce_31

action_72 _ = happyReduce_32

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn4
		 (Str happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyTerminal (TokenVarId happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 4 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVarId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Concat happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Length happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Times happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 5 4 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Tuple (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  4 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 6 4 happyReduction_19
happyReduction_19 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 4 happyReduction_20
happyReduction_20 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVarId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  4 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  4 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal (TokenNum happy_var_2))
	_
	 =  HappyAbsSyn4
		 (Proj happy_var_2 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  5 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn5
		 (TNum
	)

happyReduce_24 = happySpecReduce_1  5 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn5
		 (TBool
	)

happyReduce_25 = happySpecReduce_1  5 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn5
		 (TString
	)

happyReduce_26 = happySpecReduce_3  5 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TTuple happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  5 happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  5 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  6 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  6 happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  7 happyReduction_31
happyReduction_31 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  7 happyReduction_32
happyReduction_32 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 39 39 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 8;
	TokenTrue -> cont 9;
	TokenFalse -> cont 10;
	TokenString happy_dollar_dollar -> cont 11;
	TokenConcat -> cont 12;
	TokenLength -> cont 13;
	TokenTString -> cont 14;
	TokenPlus -> cont 15;
	TokenSub -> cont 16;
	TokenTimes -> cont 17;
	TokenAnd -> cont 18;
	TokenOr -> cont 19;
	TokenLParen -> cont 20;
	TokenRParen -> cont 21;
	TokenIf -> cont 22;
	TokenThen -> cont 23;
	TokenElse -> cont 24;
	TokenGt -> cont 25;
	TokenEq -> cont 26;
	TokenLt -> cont 27;
	TokenLambda -> cont 28;
	TokenFun -> cont 29;
	TokenColon -> cont 30;
	TokenTNum -> cont 31;
	TokenTBool -> cont 32;
	TokenComma -> cont 33;
	TokenProj -> cont 34;
	TokenLet -> cont 35;
	TokenIn -> cont 36;
	TokenEquals -> cont 37;
	TokenVarId happy_dollar_dollar -> cont 38;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 39 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
