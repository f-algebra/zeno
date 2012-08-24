{-# OPTIONS_GHC -w #-}
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), RProp,
  RType, RTerm, RAlt, RClause, REquation,
  isNameChar, 
  parseTypeDef, parseBinding, parseType,
  parseProp, parseTerm, parseClause
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Show

import qualified Zeno.Logic as Logic
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Name as Name

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn9 (RType)
	| HappyAbsSyn10 ([RType])
	| HappyAbsSyn11 (RTypeDef)
	| HappyAbsSyn12 ((String, [RType]))
	| HappyAbsSyn13 ([(String, [RType])])
	| HappyAbsSyn14 ([RVar])
	| HappyAbsSyn15 ([RAlt])
	| HappyAbsSyn16 (RAlt)
	| HappyAbsSyn17 (RVar)
	| HappyAbsSyn20 ((String, RTerm))
	| HappyAbsSyn21 (RTerm)
	| HappyAbsSyn22 (RProp)
	| HappyAbsSyn23 (Logic.Clause RVar)
	| HappyAbsSyn24 (RClause)
	| HappyAbsSyn25 (REquation)

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
 action_84,
 action_85,
 action_86 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

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
 happyReduce_39,
 happyReduce_40 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (26) = happyShift action_28
action_0 (11) = happyGoto action_27
action_0 _ = happyFail

action_1 (26) = happyShift action_25
action_1 (39) = happyShift action_26
action_1 (20) = happyGoto action_24
action_1 _ = happyFail

action_2 (26) = happyShift action_23
action_2 (22) = happyGoto action_22
action_2 _ = happyFail

action_3 (26) = happyShift action_15
action_3 (30) = happyShift action_16
action_3 (33) = happyShift action_17
action_3 (35) = happyShift action_18
action_3 (37) = happyShift action_20
action_3 (19) = happyGoto action_10
action_3 (21) = happyGoto action_21
action_3 _ = happyFail

action_4 (26) = happyShift action_15
action_4 (30) = happyShift action_16
action_4 (33) = happyShift action_17
action_4 (35) = happyShift action_18
action_4 (36) = happyShift action_19
action_4 (37) = happyShift action_20
action_4 (19) = happyGoto action_10
action_4 (21) = happyGoto action_11
action_4 (23) = happyGoto action_12
action_4 (24) = happyGoto action_13
action_4 (25) = happyGoto action_14
action_4 _ = happyFail

action_5 (26) = happyShift action_7
action_5 (30) = happyShift action_9
action_5 (9) = happyGoto action_8
action_5 _ = happyFail

action_6 (26) = happyShift action_7
action_6 _ = happyFail

action_7 _ = happyReduce_6

action_8 (29) = happyShift action_44
action_8 (41) = happyAccept
action_8 _ = happyFail

action_9 (26) = happyShift action_7
action_9 (30) = happyShift action_9
action_9 (9) = happyGoto action_43
action_9 _ = happyFail

action_10 _ = happyReduce_28

action_11 (26) = happyShift action_15
action_11 (30) = happyShift action_35
action_11 (32) = happyShift action_42
action_11 (19) = happyGoto action_34
action_11 _ = happyFail

action_12 (29) = happyShift action_41
action_12 _ = happyReduce_38

action_13 (41) = happyAccept
action_13 _ = happyFail

action_14 _ = happyReduce_37

action_15 _ = happyReduce_25

action_16 (26) = happyShift action_15
action_16 (30) = happyShift action_16
action_16 (33) = happyShift action_17
action_16 (35) = happyShift action_18
action_16 (37) = happyShift action_20
action_16 (19) = happyGoto action_10
action_16 (21) = happyGoto action_40
action_16 _ = happyFail

action_17 (26) = happyShift action_15
action_17 (30) = happyShift action_16
action_17 (33) = happyShift action_17
action_17 (35) = happyShift action_18
action_17 (37) = happyShift action_20
action_17 (19) = happyGoto action_10
action_17 (21) = happyGoto action_39
action_17 _ = happyFail

action_18 (18) = happyGoto action_38
action_18 _ = happyReduce_23

action_19 (18) = happyGoto action_37
action_19 _ = happyReduce_23

action_20 (30) = happyShift action_31
action_20 (17) = happyGoto action_36
action_20 _ = happyFail

action_21 (26) = happyShift action_15
action_21 (30) = happyShift action_35
action_21 (41) = happyAccept
action_21 (19) = happyGoto action_34
action_21 _ = happyFail

action_22 (41) = happyAccept
action_22 _ = happyFail

action_23 (32) = happyShift action_33
action_23 _ = happyFail

action_24 (41) = happyAccept
action_24 _ = happyFail

action_25 (32) = happyShift action_32
action_25 _ = happyFail

action_26 (30) = happyShift action_31
action_26 (17) = happyGoto action_30
action_26 _ = happyFail

action_27 (41) = happyAccept
action_27 _ = happyFail

action_28 (32) = happyShift action_29
action_28 _ = happyFail

action_29 (26) = happyShift action_62
action_29 (12) = happyGoto action_60
action_29 (13) = happyGoto action_61
action_29 _ = happyFail

action_30 (32) = happyShift action_59
action_30 _ = happyFail

action_31 (26) = happyShift action_58
action_31 _ = happyFail

action_32 (26) = happyShift action_15
action_32 (30) = happyShift action_16
action_32 (33) = happyShift action_17
action_32 (35) = happyShift action_18
action_32 (37) = happyShift action_20
action_32 (19) = happyGoto action_10
action_32 (21) = happyGoto action_57
action_32 _ = happyFail

action_33 (26) = happyShift action_15
action_33 (30) = happyShift action_16
action_33 (33) = happyShift action_17
action_33 (35) = happyShift action_18
action_33 (36) = happyShift action_19
action_33 (37) = happyShift action_20
action_33 (19) = happyGoto action_10
action_33 (21) = happyGoto action_11
action_33 (23) = happyGoto action_12
action_33 (24) = happyGoto action_56
action_33 (25) = happyGoto action_14
action_33 _ = happyFail

action_34 _ = happyReduce_29

action_35 (26) = happyShift action_15
action_35 (30) = happyShift action_16
action_35 (33) = happyShift action_17
action_35 (35) = happyShift action_18
action_35 (37) = happyShift action_20
action_35 (19) = happyGoto action_10
action_35 (21) = happyGoto action_55
action_35 _ = happyFail

action_36 (40) = happyShift action_54
action_36 _ = happyFail

action_37 (29) = happyShift action_53
action_37 (30) = happyShift action_31
action_37 (17) = happyGoto action_51
action_37 _ = happyFail

action_38 (29) = happyShift action_52
action_38 (30) = happyShift action_31
action_38 (17) = happyGoto action_51
action_38 _ = happyFail

action_39 (26) = happyShift action_15
action_39 (30) = happyShift action_35
action_39 (34) = happyShift action_50
action_39 (19) = happyGoto action_34
action_39 _ = happyFail

action_40 (26) = happyShift action_15
action_40 (30) = happyShift action_35
action_40 (31) = happyShift action_49
action_40 (19) = happyGoto action_34
action_40 _ = happyFail

action_41 (26) = happyShift action_15
action_41 (30) = happyShift action_16
action_41 (33) = happyShift action_17
action_41 (35) = happyShift action_18
action_41 (37) = happyShift action_20
action_41 (19) = happyGoto action_10
action_41 (21) = happyGoto action_11
action_41 (25) = happyGoto action_48
action_41 _ = happyFail

action_42 (26) = happyShift action_15
action_42 (30) = happyShift action_16
action_42 (33) = happyShift action_17
action_42 (35) = happyShift action_18
action_42 (37) = happyShift action_20
action_42 (19) = happyGoto action_10
action_42 (21) = happyGoto action_47
action_42 _ = happyFail

action_43 (29) = happyShift action_44
action_43 (31) = happyShift action_46
action_43 _ = happyFail

action_44 (26) = happyShift action_7
action_44 (30) = happyShift action_9
action_44 (9) = happyGoto action_45
action_44 _ = happyFail

action_45 (29) = happyShift action_44
action_45 _ = happyReduce_8

action_46 _ = happyReduce_7

action_47 (26) = happyShift action_15
action_47 (30) = happyShift action_35
action_47 (19) = happyGoto action_34
action_47 _ = happyReduce_40

action_48 _ = happyReduce_36

action_49 _ = happyReduce_31

action_50 (26) = happyShift action_15
action_50 (27) = happyShift action_76
action_50 (14) = happyGoto action_72
action_50 (15) = happyGoto action_73
action_50 (16) = happyGoto action_74
action_50 (19) = happyGoto action_75
action_50 _ = happyFail

action_51 _ = happyReduce_24

action_52 (26) = happyShift action_15
action_52 (30) = happyShift action_16
action_52 (33) = happyShift action_17
action_52 (35) = happyShift action_18
action_52 (37) = happyShift action_20
action_52 (19) = happyGoto action_10
action_52 (21) = happyGoto action_71
action_52 _ = happyFail

action_53 (26) = happyShift action_15
action_53 (30) = happyShift action_16
action_53 (33) = happyShift action_17
action_53 (35) = happyShift action_18
action_53 (37) = happyShift action_20
action_53 (19) = happyGoto action_10
action_53 (21) = happyGoto action_11
action_53 (23) = happyGoto action_70
action_53 (25) = happyGoto action_14
action_53 _ = happyFail

action_54 (26) = happyShift action_15
action_54 (30) = happyShift action_16
action_54 (33) = happyShift action_17
action_54 (35) = happyShift action_18
action_54 (37) = happyShift action_20
action_54 (19) = happyGoto action_10
action_54 (21) = happyGoto action_69
action_54 _ = happyFail

action_55 (26) = happyShift action_15
action_55 (30) = happyShift action_35
action_55 (31) = happyShift action_68
action_55 (19) = happyGoto action_34
action_55 _ = happyFail

action_56 _ = happyReduce_35

action_57 (26) = happyShift action_15
action_57 (30) = happyShift action_35
action_57 (19) = happyGoto action_34
action_57 _ = happyReduce_26

action_58 (28) = happyShift action_67
action_58 _ = happyFail

action_59 (26) = happyShift action_15
action_59 (30) = happyShift action_16
action_59 (33) = happyShift action_17
action_59 (35) = happyShift action_18
action_59 (37) = happyShift action_20
action_59 (19) = happyGoto action_10
action_59 (21) = happyGoto action_66
action_59 _ = happyFail

action_60 _ = happyReduce_14

action_61 (27) = happyShift action_65
action_61 _ = happyReduce_11

action_62 (26) = happyShift action_7
action_62 (30) = happyShift action_9
action_62 (9) = happyGoto action_63
action_62 (10) = happyGoto action_64
action_62 _ = happyReduce_12

action_63 (29) = happyShift action_44
action_63 _ = happyReduce_9

action_64 (26) = happyShift action_7
action_64 (30) = happyShift action_9
action_64 (9) = happyGoto action_83
action_64 _ = happyReduce_13

action_65 (26) = happyShift action_62
action_65 (12) = happyGoto action_82
action_65 _ = happyFail

action_66 (26) = happyShift action_15
action_66 (30) = happyShift action_35
action_66 (19) = happyGoto action_34
action_66 _ = happyReduce_27

action_67 (26) = happyShift action_7
action_67 (30) = happyShift action_9
action_67 (9) = happyGoto action_81
action_67 _ = happyFail

action_68 _ = happyReduce_30

action_69 (26) = happyShift action_15
action_69 (30) = happyShift action_35
action_69 (19) = happyGoto action_34
action_69 _ = happyReduce_33

action_70 (29) = happyShift action_41
action_70 _ = happyReduce_39

action_71 (26) = happyShift action_15
action_71 (30) = happyShift action_35
action_71 (19) = happyGoto action_34
action_71 _ = happyReduce_32

action_72 (26) = happyShift action_15
action_72 (29) = happyShift action_80
action_72 (19) = happyGoto action_79
action_72 _ = happyFail

action_73 (27) = happyShift action_78
action_73 _ = happyReduce_34

action_74 _ = happyReduce_18

action_75 _ = happyReduce_16

action_76 (26) = happyShift action_15
action_76 (14) = happyGoto action_72
action_76 (16) = happyGoto action_77
action_76 (19) = happyGoto action_75
action_76 _ = happyFail

action_77 _ = happyReduce_19

action_78 (26) = happyShift action_15
action_78 (14) = happyGoto action_72
action_78 (16) = happyGoto action_86
action_78 (19) = happyGoto action_75
action_78 _ = happyFail

action_79 _ = happyReduce_17

action_80 (26) = happyShift action_15
action_80 (30) = happyShift action_16
action_80 (33) = happyShift action_17
action_80 (35) = happyShift action_18
action_80 (37) = happyShift action_20
action_80 (19) = happyGoto action_10
action_80 (21) = happyGoto action_85
action_80 _ = happyFail

action_81 (29) = happyShift action_44
action_81 (31) = happyShift action_84
action_81 _ = happyFail

action_82 _ = happyReduce_15

action_83 (29) = happyShift action_44
action_83 _ = happyReduce_10

action_84 _ = happyReduce_22

action_85 (26) = happyShift action_15
action_85 (30) = happyShift action_35
action_85 (19) = happyGoto action_34
action_85 _ = happyReduce_21

action_86 _ = happyReduce_20

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 (Type.Var happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  9 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Type.Fun happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  10 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  10 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn11
		 (RTypeDef happy_var_1
                                          (map (mkTypeDefCon happy_var_1) happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, [])
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  12 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_2)
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  14 happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  15 happyReduction_18
happyReduction_18 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  15 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 ([happy_var_2]
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 (Term.Alt (head happy_var_1) (tail happy_var_1) happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 17 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (RVar happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_0  18 happyReduction_23
happyReduction_23  =  HappyAbsSyn14
		 ([]
	)

happyReduce_24 = happySpecReduce_2  18 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  19 happyReduction_25
happyReduction_25 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn17
		 (RVar happy_var_1 Nothing
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  20 happyReduction_26
happyReduction_26 (HappyAbsSyn21  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn20
		 ((happy_var_1, happy_var_3)
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 20 happyReduction_27
happyReduction_27 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((varName happy_var_2, Term.Fix happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  21 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Term.Var happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  21 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Term.App happy_var_1 (Term.Var happy_var_2)
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 21 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Term.App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  21 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 21 happyReduction_32
happyReduction_32 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Term.unflattenLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4 21 happyReduction_33
happyReduction_33 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Term.Fix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 21 happyReduction_34
happyReduction_34 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Term.Cse undefined happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  22 happyReduction_35
happyReduction_35 (HappyAbsSyn24  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  23 happyReduction_36
happyReduction_36 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (Logic.Clause (Logic.flatten happy_var_1) happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  23 happyReduction_37
happyReduction_37 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn23
		 (Logic.Clause [] happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  24 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 (([], happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 24 happyReduction_39
happyReduction_39 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  25 happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (Logic.Equal happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 26;
	TokenBar -> cont 27;
	TokenType -> cont 28;
	TokenArr -> cont 29;
	TokenOP -> cont 30;
	TokenCP -> cont 31;
	TokenEq -> cont 32;
	TokenCase -> cont 33;
	TokenOf -> cont 34;
	TokenLambda -> cont 35;
	TokenAll -> cont 36;
	TokenFix -> cont 37;
	TokenLet -> cont 38;
	TokenRec -> cont 39;
	TokenIn -> cont 40;
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

binding tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

prop tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

term tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

clause tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

typ tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

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

parseType :: String -> RType
parseType = typ . lexer

instance Show RTypeDef where
  show (RTypeDef name cons) = 
    "type " ++ name ++ " where" ++ cons_s
    where
    cons_s = concatMap (mappend "\n  " . showCon) cons
    showCon (name, typ) = name ++ " : " ++ show typ
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

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

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
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

notHappyAtAll :: a
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

{-# LINE 311 "templates\\GenericTemplate.hs" #-}
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
