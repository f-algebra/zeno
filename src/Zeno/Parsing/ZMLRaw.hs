{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), RProp,
  RType, RTerm, RAlt, RClause, REquation,
  isNameChar, 
  parseTypeDef, parseBinding, 
  parseProp, parseTerm, parseClause
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Show

import qualified Zeno.Logic as Logic
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Name as Name

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn8 (RType)
	| HappyAbsSyn9 ([RType])
	| HappyAbsSyn10 (RTypeDef)
	| HappyAbsSyn11 ((String, [RType]))
	| HappyAbsSyn12 ([(String, [RType])])
	| HappyAbsSyn13 ([RVar])
	| HappyAbsSyn14 ([RAlt])
	| HappyAbsSyn15 (RAlt)
	| HappyAbsSyn16 (RVar)
	| HappyAbsSyn19 ((String, RTerm))
	| HappyAbsSyn20 (RTerm)
	| HappyAbsSyn21 (RProp)
	| HappyAbsSyn22 (Logic.Clause RVar)
	| HappyAbsSyn23 (RClause)
	| HappyAbsSyn24 (REquation)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (25) = happyShift action_25
action_0 (10) = happyGoto action_24
action_0 _ = happyFail

action_1 (25) = happyShift action_22
action_1 (38) = happyShift action_23
action_1 (19) = happyGoto action_21
action_1 _ = happyFail

action_2 (25) = happyShift action_20
action_2 (21) = happyGoto action_19
action_2 _ = happyFail

action_3 (25) = happyShift action_12
action_3 (29) = happyShift action_13
action_3 (32) = happyShift action_14
action_3 (34) = happyShift action_15
action_3 (36) = happyShift action_17
action_3 (18) = happyGoto action_7
action_3 (20) = happyGoto action_18
action_3 _ = happyFail

action_4 (25) = happyShift action_12
action_4 (29) = happyShift action_13
action_4 (32) = happyShift action_14
action_4 (34) = happyShift action_15
action_4 (35) = happyShift action_16
action_4 (36) = happyShift action_17
action_4 (18) = happyGoto action_7
action_4 (20) = happyGoto action_8
action_4 (22) = happyGoto action_9
action_4 (23) = happyGoto action_10
action_4 (24) = happyGoto action_11
action_4 _ = happyFail

action_5 (25) = happyShift action_6
action_5 _ = happyFail

action_6 _ = happyReduce_5

action_7 _ = happyReduce_27

action_8 (25) = happyShift action_12
action_8 (29) = happyShift action_32
action_8 (31) = happyShift action_39
action_8 (18) = happyGoto action_31
action_8 _ = happyFail

action_9 (28) = happyShift action_38
action_9 _ = happyReduce_37

action_10 (40) = happyAccept
action_10 _ = happyFail

action_11 _ = happyReduce_36

action_12 _ = happyReduce_24

action_13 (25) = happyShift action_12
action_13 (29) = happyShift action_13
action_13 (32) = happyShift action_14
action_13 (34) = happyShift action_15
action_13 (36) = happyShift action_17
action_13 (18) = happyGoto action_7
action_13 (20) = happyGoto action_37
action_13 _ = happyFail

action_14 (25) = happyShift action_12
action_14 (29) = happyShift action_13
action_14 (32) = happyShift action_14
action_14 (34) = happyShift action_15
action_14 (36) = happyShift action_17
action_14 (18) = happyGoto action_7
action_14 (20) = happyGoto action_36
action_14 _ = happyFail

action_15 (17) = happyGoto action_35
action_15 _ = happyReduce_22

action_16 (17) = happyGoto action_34
action_16 _ = happyReduce_22

action_17 (29) = happyShift action_28
action_17 (16) = happyGoto action_33
action_17 _ = happyFail

action_18 (25) = happyShift action_12
action_18 (29) = happyShift action_32
action_18 (40) = happyAccept
action_18 (18) = happyGoto action_31
action_18 _ = happyFail

action_19 (40) = happyAccept
action_19 _ = happyFail

action_20 (31) = happyShift action_30
action_20 _ = happyFail

action_21 (40) = happyAccept
action_21 _ = happyFail

action_22 (31) = happyShift action_29
action_22 _ = happyFail

action_23 (29) = happyShift action_28
action_23 (16) = happyGoto action_27
action_23 _ = happyFail

action_24 (40) = happyAccept
action_24 _ = happyFail

action_25 (31) = happyShift action_26
action_25 _ = happyFail

action_26 (25) = happyShift action_55
action_26 (11) = happyGoto action_53
action_26 (12) = happyGoto action_54
action_26 _ = happyFail

action_27 (31) = happyShift action_52
action_27 _ = happyFail

action_28 (25) = happyShift action_51
action_28 _ = happyFail

action_29 (25) = happyShift action_12
action_29 (29) = happyShift action_13
action_29 (32) = happyShift action_14
action_29 (34) = happyShift action_15
action_29 (36) = happyShift action_17
action_29 (18) = happyGoto action_7
action_29 (20) = happyGoto action_50
action_29 _ = happyFail

action_30 (25) = happyShift action_12
action_30 (29) = happyShift action_13
action_30 (32) = happyShift action_14
action_30 (34) = happyShift action_15
action_30 (35) = happyShift action_16
action_30 (36) = happyShift action_17
action_30 (18) = happyGoto action_7
action_30 (20) = happyGoto action_8
action_30 (22) = happyGoto action_9
action_30 (23) = happyGoto action_49
action_30 (24) = happyGoto action_11
action_30 _ = happyFail

action_31 _ = happyReduce_28

action_32 (25) = happyShift action_12
action_32 (29) = happyShift action_13
action_32 (32) = happyShift action_14
action_32 (34) = happyShift action_15
action_32 (36) = happyShift action_17
action_32 (18) = happyGoto action_7
action_32 (20) = happyGoto action_48
action_32 _ = happyFail

action_33 (39) = happyShift action_47
action_33 _ = happyFail

action_34 (28) = happyShift action_46
action_34 (29) = happyShift action_28
action_34 (16) = happyGoto action_44
action_34 _ = happyFail

action_35 (28) = happyShift action_45
action_35 (29) = happyShift action_28
action_35 (16) = happyGoto action_44
action_35 _ = happyFail

action_36 (25) = happyShift action_12
action_36 (29) = happyShift action_32
action_36 (33) = happyShift action_43
action_36 (18) = happyGoto action_31
action_36 _ = happyFail

action_37 (25) = happyShift action_12
action_37 (29) = happyShift action_32
action_37 (30) = happyShift action_42
action_37 (18) = happyGoto action_31
action_37 _ = happyFail

action_38 (25) = happyShift action_12
action_38 (29) = happyShift action_13
action_38 (32) = happyShift action_14
action_38 (34) = happyShift action_15
action_38 (36) = happyShift action_17
action_38 (18) = happyGoto action_7
action_38 (20) = happyGoto action_8
action_38 (24) = happyGoto action_41
action_38 _ = happyFail

action_39 (25) = happyShift action_12
action_39 (29) = happyShift action_13
action_39 (32) = happyShift action_14
action_39 (34) = happyShift action_15
action_39 (36) = happyShift action_17
action_39 (18) = happyGoto action_7
action_39 (20) = happyGoto action_40
action_39 _ = happyFail

action_40 (25) = happyShift action_12
action_40 (29) = happyShift action_32
action_40 (18) = happyGoto action_31
action_40 _ = happyReduce_39

action_41 _ = happyReduce_35

action_42 _ = happyReduce_30

action_43 (25) = happyShift action_12
action_43 (26) = happyShift action_70
action_43 (13) = happyGoto action_66
action_43 (14) = happyGoto action_67
action_43 (15) = happyGoto action_68
action_43 (18) = happyGoto action_69
action_43 _ = happyFail

action_44 _ = happyReduce_23

action_45 (25) = happyShift action_12
action_45 (29) = happyShift action_13
action_45 (32) = happyShift action_14
action_45 (34) = happyShift action_15
action_45 (36) = happyShift action_17
action_45 (18) = happyGoto action_7
action_45 (20) = happyGoto action_65
action_45 _ = happyFail

action_46 (25) = happyShift action_12
action_46 (29) = happyShift action_13
action_46 (32) = happyShift action_14
action_46 (34) = happyShift action_15
action_46 (36) = happyShift action_17
action_46 (18) = happyGoto action_7
action_46 (20) = happyGoto action_8
action_46 (22) = happyGoto action_64
action_46 (24) = happyGoto action_11
action_46 _ = happyFail

action_47 (25) = happyShift action_12
action_47 (29) = happyShift action_13
action_47 (32) = happyShift action_14
action_47 (34) = happyShift action_15
action_47 (36) = happyShift action_17
action_47 (18) = happyGoto action_7
action_47 (20) = happyGoto action_63
action_47 _ = happyFail

action_48 (25) = happyShift action_12
action_48 (29) = happyShift action_32
action_48 (30) = happyShift action_62
action_48 (18) = happyGoto action_31
action_48 _ = happyFail

action_49 _ = happyReduce_34

action_50 (25) = happyShift action_12
action_50 (29) = happyShift action_32
action_50 (18) = happyGoto action_31
action_50 _ = happyReduce_25

action_51 (27) = happyShift action_61
action_51 _ = happyFail

action_52 (25) = happyShift action_12
action_52 (29) = happyShift action_13
action_52 (32) = happyShift action_14
action_52 (34) = happyShift action_15
action_52 (36) = happyShift action_17
action_52 (18) = happyGoto action_7
action_52 (20) = happyGoto action_60
action_52 _ = happyFail

action_53 _ = happyReduce_13

action_54 (26) = happyShift action_59
action_54 _ = happyReduce_10

action_55 (25) = happyShift action_6
action_55 (29) = happyShift action_58
action_55 (8) = happyGoto action_56
action_55 (9) = happyGoto action_57
action_55 _ = happyReduce_11

action_56 (28) = happyShift action_79
action_56 _ = happyReduce_8

action_57 (25) = happyShift action_6
action_57 (29) = happyShift action_58
action_57 (8) = happyGoto action_78
action_57 _ = happyReduce_12

action_58 (25) = happyShift action_6
action_58 (29) = happyShift action_58
action_58 (8) = happyGoto action_77
action_58 _ = happyFail

action_59 (25) = happyShift action_55
action_59 (11) = happyGoto action_76
action_59 _ = happyFail

action_60 (25) = happyShift action_12
action_60 (29) = happyShift action_32
action_60 (18) = happyGoto action_31
action_60 _ = happyReduce_26

action_61 (25) = happyShift action_6
action_61 (29) = happyShift action_58
action_61 (8) = happyGoto action_75
action_61 _ = happyFail

action_62 _ = happyReduce_29

action_63 (25) = happyShift action_12
action_63 (29) = happyShift action_32
action_63 (18) = happyGoto action_31
action_63 _ = happyReduce_32

action_64 (28) = happyShift action_38
action_64 _ = happyReduce_38

action_65 (25) = happyShift action_12
action_65 (29) = happyShift action_32
action_65 (18) = happyGoto action_31
action_65 _ = happyReduce_31

action_66 (25) = happyShift action_12
action_66 (28) = happyShift action_74
action_66 (18) = happyGoto action_73
action_66 _ = happyFail

action_67 (26) = happyShift action_72
action_67 _ = happyReduce_33

action_68 _ = happyReduce_17

action_69 _ = happyReduce_15

action_70 (25) = happyShift action_12
action_70 (13) = happyGoto action_66
action_70 (15) = happyGoto action_71
action_70 (18) = happyGoto action_69
action_70 _ = happyFail

action_71 _ = happyReduce_18

action_72 (25) = happyShift action_12
action_72 (13) = happyGoto action_66
action_72 (15) = happyGoto action_84
action_72 (18) = happyGoto action_69
action_72 _ = happyFail

action_73 _ = happyReduce_16

action_74 (25) = happyShift action_12
action_74 (29) = happyShift action_13
action_74 (32) = happyShift action_14
action_74 (34) = happyShift action_15
action_74 (36) = happyShift action_17
action_74 (18) = happyGoto action_7
action_74 (20) = happyGoto action_83
action_74 _ = happyFail

action_75 (28) = happyShift action_79
action_75 (30) = happyShift action_82
action_75 _ = happyFail

action_76 _ = happyReduce_14

action_77 (28) = happyShift action_79
action_77 (30) = happyShift action_81
action_77 _ = happyFail

action_78 (28) = happyShift action_79
action_78 _ = happyReduce_9

action_79 (25) = happyShift action_6
action_79 (29) = happyShift action_58
action_79 (8) = happyGoto action_80
action_79 _ = happyFail

action_80 (28) = happyShift action_79
action_80 _ = happyReduce_7

action_81 _ = happyReduce_6

action_82 _ = happyReduce_21

action_83 (25) = happyShift action_12
action_83 (29) = happyShift action_32
action_83 (18) = happyGoto action_31
action_83 _ = happyReduce_20

action_84 _ = happyReduce_19

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn8
		 (Type.Var happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Type.Fun happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  10 happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 (RTypeDef happy_var_1
                                          (map (mkTypeDefCon happy_var_1) happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1, [])
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_2)
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_2)
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 ([happy_var_2]
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (Term.Alt (head happy_var_1) (tail happy_var_1) happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 16 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (RVar happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_0  17 happyReduction_22
happyReduction_22  =  HappyAbsSyn13
		 ([]
	)

happyReduce_23 = happySpecReduce_2  17 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  18 happyReduction_24
happyReduction_24 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn16
		 (RVar happy_var_1 Nothing
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  19 happyReduction_25
happyReduction_25 (HappyAbsSyn20  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn19
		 ((happy_var_1, happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 19 happyReduction_26
happyReduction_26 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((varName happy_var_2, Term.Fix happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  20 happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn20
		 (Term.Var happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  20 happyReduction_28
happyReduction_28 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Term.App happy_var_1 (Term.Var happy_var_2)
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 20 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Term.App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  20 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 20 happyReduction_31
happyReduction_31 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Term.unflattenLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 4 20 happyReduction_32
happyReduction_32 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Term.Fix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4 20 happyReduction_33
happyReduction_33 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Term.Cse Nothing happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 (HappyAbsSyn23  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn21
		 ((happy_var_1, happy_var_3)
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  22 happyReduction_35
happyReduction_35 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (Logic.Clause (Logic.flatten happy_var_1) happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn22
		 (Logic.Clause [] happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn23
		 (([], happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happyReduce 4 23 happyReduction_38
happyReduction_38 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_3  24 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn24
		 (Logic.Equal happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 25;
	TokenBar -> cont 26;
	TokenType -> cont 27;
	TokenArr -> cont 28;
	TokenOP -> cont 29;
	TokenCP -> cont 30;
	TokenEq -> cont 31;
	TokenCase -> cont 32;
	TokenOf -> cont 33;
	TokenLambda -> cont 34;
	TokenAll -> cont 35;
	TokenFix -> cont 36;
	TokenLet -> cont 37;
	TokenRec -> cont 38;
	TokenIn -> cont 39;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

typeDef tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

binding tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

prop tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

term tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

clause tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term RVar
type RAlt = Term.Alt RVar
type RClause = ([RVar], Logic.Clause RVar)
type REquation = Logic.Equation RVar
type RProp = (String, RClause)
data RVar
  = RVar      { varName :: String, 
                varType :: Maybe RType }
                
data RTypeDef 
  = RTypeDef  { typeName :: String,
                typeCons :: [(String, RType)] }
  
mkTypeDefCon :: String -> (String, [RType]) -> (String, RType)
mkTypeDefCon result_type (name, arg_types) = 
  (name, Type.unflatten (arg_types ++ [Type.Var result_type]))

data Token
  = TokenBar
  | TokenName String
  | TokenType
  | TokenArr
  | TokenOP
  | TokenCP
  | TokenEq
  | TokenCase
  | TokenOf
  | TokenLambda
  | TokenAll
  | TokenFix
  | TokenLet
  | TokenRec
  | TokenIn
  deriving Show
  
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_"
  
lexer :: String -> [Token]
lexer [] = []
lexer ('/':'*':cs) = lexer (commentEnd cs)
  where
  commentEnd [] = []
  commentEnd ('*':'/':cs) = cs
  commentEnd (c:cs) = commentEnd cs
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('-':'>':cs) = TokenArr : lexer cs
lexer (':':cs) = TokenType : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer (c:cs) 
  | isSpace c = lexer cs
  | isNameChar c = lexVar (c : cs)
  where
  lexVar cs =
    case span isNameChar cs of
      ("fun", rest) -> TokenLambda : lexer rest
      ("fix", rest) -> TokenFix : lexer rest
      ("case", rest) -> TokenCase : lexer rest
      ("of", rest) -> TokenOf : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      ("rec", rest) -> TokenRec : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("all", rest) -> TokenAll : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

parseTypeDef :: String -> RTypeDef
parseTypeDef = typeDef . lexer

parseBinding :: String -> (String, RTerm)
parseBinding = binding . lexer

parseProp :: String -> RProp
parseProp = prop . lexer

parseTerm :: String -> RTerm
parseTerm = term . lexer

parseClause :: String -> RClause
parseClause = clause . lexer

instance Show RTypeDef where
  show (RTypeDef name cons) = 
    "type " ++ name ++ " where" ++ cons_s
    where
    cons_s = concatMap (mappend "\n  " . showCon) cons
    showCon (name, typ) = name ++ " : " ++ show typ
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
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
