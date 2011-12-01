{-# OPTIONS_GHC -w #-}
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), RProp (..),
  RType, RTerm, RAlt, RClause, REquation,
  isNameChar, 
  parseTypeDef, parseBinding, 
  parseProp, parseTerm
) where
import Prelude ()
import Zeno.Prelude
import Zeno.Show

import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Name as Name

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 (RType)
	| HappyAbsSyn8 ([RType])
	| HappyAbsSyn9 (RTypeDef)
	| HappyAbsSyn10 ((String, [RType]))
	| HappyAbsSyn11 ([(String, [RType])])
	| HappyAbsSyn12 ([RVar])
	| HappyAbsSyn13 ([RAlt])
	| HappyAbsSyn14 (RAlt)
	| HappyAbsSyn15 (RVar)
	| HappyAbsSyn18 ((String, RTerm))
	| HappyAbsSyn19 (RTerm)
	| HappyAbsSyn20 (RProp)
	| HappyAbsSyn21 (RClause)
	| HappyAbsSyn22 (REquation)

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
 action_76 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_4,
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
 happyReduce_35 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (23) = happyShift action_19
action_0 (9) = happyGoto action_18
action_0 _ = happyFail

action_1 (23) = happyShift action_16
action_1 (35) = happyShift action_17
action_1 (18) = happyGoto action_15
action_1 _ = happyFail

action_2 (23) = happyShift action_14
action_2 (20) = happyGoto action_13
action_2 _ = happyFail

action_3 (23) = happyShift action_8
action_3 (27) = happyShift action_9
action_3 (30) = happyShift action_10
action_3 (32) = happyShift action_11
action_3 (33) = happyShift action_12
action_3 (17) = happyGoto action_6
action_3 (19) = happyGoto action_7
action_3 _ = happyFail

action_4 (23) = happyShift action_5
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 _ = happyReduce_25

action_7 (23) = happyShift action_8
action_7 (27) = happyShift action_30
action_7 (37) = happyAccept
action_7 (17) = happyGoto action_29
action_7 _ = happyFail

action_8 _ = happyReduce_22

action_9 (23) = happyShift action_8
action_9 (27) = happyShift action_9
action_9 (30) = happyShift action_10
action_9 (32) = happyShift action_11
action_9 (33) = happyShift action_12
action_9 (17) = happyGoto action_6
action_9 (19) = happyGoto action_28
action_9 _ = happyFail

action_10 (23) = happyShift action_8
action_10 (27) = happyShift action_9
action_10 (30) = happyShift action_10
action_10 (32) = happyShift action_11
action_10 (33) = happyShift action_12
action_10 (17) = happyGoto action_6
action_10 (19) = happyGoto action_27
action_10 _ = happyFail

action_11 (27) = happyShift action_22
action_11 (15) = happyGoto action_26
action_11 _ = happyFail

action_12 (27) = happyShift action_22
action_12 (15) = happyGoto action_25
action_12 _ = happyFail

action_13 (37) = happyAccept
action_13 _ = happyFail

action_14 (16) = happyGoto action_24
action_14 _ = happyReduce_20

action_15 (37) = happyAccept
action_15 _ = happyFail

action_16 (29) = happyShift action_23
action_16 _ = happyFail

action_17 (27) = happyShift action_22
action_17 (15) = happyGoto action_21
action_17 _ = happyFail

action_18 (37) = happyAccept
action_18 _ = happyFail

action_19 (29) = happyShift action_20
action_19 _ = happyFail

action_20 (23) = happyShift action_43
action_20 (10) = happyGoto action_41
action_20 (11) = happyGoto action_42
action_20 _ = happyFail

action_21 (29) = happyShift action_40
action_21 _ = happyFail

action_22 (23) = happyShift action_39
action_22 _ = happyFail

action_23 (23) = happyShift action_8
action_23 (27) = happyShift action_9
action_23 (30) = happyShift action_10
action_23 (32) = happyShift action_11
action_23 (33) = happyShift action_12
action_23 (17) = happyGoto action_6
action_23 (19) = happyGoto action_38
action_23 _ = happyFail

action_24 (27) = happyShift action_22
action_24 (29) = happyShift action_37
action_24 (15) = happyGoto action_36
action_24 _ = happyFail

action_25 (36) = happyShift action_35
action_25 _ = happyFail

action_26 (26) = happyShift action_34
action_26 _ = happyFail

action_27 (23) = happyShift action_8
action_27 (27) = happyShift action_30
action_27 (31) = happyShift action_33
action_27 (17) = happyGoto action_29
action_27 _ = happyFail

action_28 (23) = happyShift action_8
action_28 (27) = happyShift action_30
action_28 (28) = happyShift action_32
action_28 (17) = happyGoto action_29
action_28 _ = happyFail

action_29 _ = happyReduce_26

action_30 (23) = happyShift action_8
action_30 (27) = happyShift action_9
action_30 (30) = happyShift action_10
action_30 (32) = happyShift action_11
action_30 (33) = happyShift action_12
action_30 (17) = happyGoto action_6
action_30 (19) = happyGoto action_31
action_30 _ = happyFail

action_31 (23) = happyShift action_8
action_31 (27) = happyShift action_30
action_31 (28) = happyShift action_59
action_31 (17) = happyGoto action_29
action_31 _ = happyFail

action_32 _ = happyReduce_28

action_33 (23) = happyShift action_8
action_33 (12) = happyGoto action_55
action_33 (13) = happyGoto action_56
action_33 (14) = happyGoto action_57
action_33 (17) = happyGoto action_58
action_33 _ = happyFail

action_34 (23) = happyShift action_8
action_34 (27) = happyShift action_9
action_34 (30) = happyShift action_10
action_34 (32) = happyShift action_11
action_34 (33) = happyShift action_12
action_34 (17) = happyGoto action_6
action_34 (19) = happyGoto action_54
action_34 _ = happyFail

action_35 (23) = happyShift action_8
action_35 (27) = happyShift action_9
action_35 (30) = happyShift action_10
action_35 (32) = happyShift action_11
action_35 (33) = happyShift action_12
action_35 (17) = happyGoto action_6
action_35 (19) = happyGoto action_53
action_35 _ = happyFail

action_36 _ = happyReduce_21

action_37 (23) = happyShift action_8
action_37 (27) = happyShift action_9
action_37 (30) = happyShift action_10
action_37 (32) = happyShift action_11
action_37 (33) = happyShift action_12
action_37 (17) = happyGoto action_6
action_37 (19) = happyGoto action_50
action_37 (21) = happyGoto action_51
action_37 (22) = happyGoto action_52
action_37 _ = happyFail

action_38 (23) = happyShift action_8
action_38 (27) = happyShift action_30
action_38 (17) = happyGoto action_29
action_38 _ = happyReduce_23

action_39 (25) = happyShift action_49
action_39 _ = happyFail

action_40 (23) = happyShift action_8
action_40 (27) = happyShift action_9
action_40 (30) = happyShift action_10
action_40 (32) = happyShift action_11
action_40 (33) = happyShift action_12
action_40 (17) = happyGoto action_6
action_40 (19) = happyGoto action_48
action_40 _ = happyFail

action_41 _ = happyReduce_12

action_42 (24) = happyShift action_47
action_42 _ = happyReduce_9

action_43 (23) = happyShift action_5
action_43 (27) = happyShift action_46
action_43 (7) = happyGoto action_44
action_43 (8) = happyGoto action_45
action_43 _ = happyReduce_10

action_44 (26) = happyShift action_69
action_44 _ = happyReduce_7

action_45 (23) = happyShift action_5
action_45 (27) = happyShift action_46
action_45 (7) = happyGoto action_68
action_45 _ = happyReduce_11

action_46 (23) = happyShift action_5
action_46 (27) = happyShift action_46
action_46 (7) = happyGoto action_67
action_46 _ = happyFail

action_47 (23) = happyShift action_43
action_47 (10) = happyGoto action_66
action_47 _ = happyFail

action_48 (23) = happyShift action_8
action_48 (27) = happyShift action_30
action_48 (17) = happyGoto action_29
action_48 _ = happyReduce_24

action_49 (23) = happyShift action_5
action_49 (27) = happyShift action_46
action_49 (7) = happyGoto action_65
action_49 _ = happyFail

action_50 (23) = happyShift action_8
action_50 (27) = happyShift action_30
action_50 (29) = happyShift action_64
action_50 (17) = happyGoto action_29
action_50 _ = happyFail

action_51 (26) = happyShift action_63
action_51 _ = happyReduce_32

action_52 _ = happyReduce_34

action_53 (23) = happyShift action_8
action_53 (27) = happyShift action_30
action_53 (17) = happyGoto action_29
action_53 _ = happyReduce_30

action_54 (23) = happyShift action_8
action_54 (27) = happyShift action_30
action_54 (17) = happyGoto action_29
action_54 _ = happyReduce_29

action_55 (23) = happyShift action_8
action_55 (26) = happyShift action_62
action_55 (17) = happyGoto action_61
action_55 _ = happyFail

action_56 (24) = happyShift action_60
action_56 _ = happyReduce_31

action_57 _ = happyReduce_16

action_58 _ = happyReduce_14

action_59 _ = happyReduce_27

action_60 (23) = happyShift action_8
action_60 (12) = happyGoto action_55
action_60 (14) = happyGoto action_76
action_60 (17) = happyGoto action_58
action_60 _ = happyFail

action_61 _ = happyReduce_15

action_62 (23) = happyShift action_8
action_62 (27) = happyShift action_9
action_62 (30) = happyShift action_10
action_62 (32) = happyShift action_11
action_62 (33) = happyShift action_12
action_62 (17) = happyGoto action_6
action_62 (19) = happyGoto action_75
action_62 _ = happyFail

action_63 (23) = happyShift action_8
action_63 (27) = happyShift action_9
action_63 (30) = happyShift action_10
action_63 (32) = happyShift action_11
action_63 (33) = happyShift action_12
action_63 (17) = happyGoto action_6
action_63 (19) = happyGoto action_50
action_63 (22) = happyGoto action_74
action_63 _ = happyFail

action_64 (23) = happyShift action_8
action_64 (27) = happyShift action_9
action_64 (30) = happyShift action_10
action_64 (32) = happyShift action_11
action_64 (33) = happyShift action_12
action_64 (17) = happyGoto action_6
action_64 (19) = happyGoto action_73
action_64 _ = happyFail

action_65 (26) = happyShift action_69
action_65 (28) = happyShift action_72
action_65 _ = happyFail

action_66 _ = happyReduce_13

action_67 (26) = happyShift action_69
action_67 (28) = happyShift action_71
action_67 _ = happyFail

action_68 (26) = happyShift action_69
action_68 _ = happyReduce_8

action_69 (23) = happyShift action_5
action_69 (27) = happyShift action_46
action_69 (7) = happyGoto action_70
action_69 _ = happyFail

action_70 (26) = happyShift action_69
action_70 _ = happyReduce_6

action_71 _ = happyReduce_5

action_72 _ = happyReduce_19

action_73 (23) = happyShift action_8
action_73 (27) = happyShift action_30
action_73 (17) = happyGoto action_29
action_73 _ = happyReduce_35

action_74 _ = happyReduce_33

action_75 (23) = happyShift action_8
action_75 (27) = happyShift action_30
action_75 (17) = happyGoto action_29
action_75 _ = happyReduce_18

action_76 _ = happyReduce_17

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn7
		 (Type.Var happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Type.Fun happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn9
		 (RTypeDef happy_var_1
                                          (map (mkTypeDefCon happy_var_1) happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 ((happy_var_1, [])
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn10
		 ((happy_var_1, happy_var_2)
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  12 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  14 happyReduction_18
happyReduction_18 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn14
		 (Term.Alt (head happy_var_1) (tail happy_var_1) happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 15 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (RVar happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_0  16 happyReduction_20
happyReduction_20  =  HappyAbsSyn12
		 ([]
	)

happyReduce_21 = happySpecReduce_2  16 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  17 happyReduction_22
happyReduction_22 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn15
		 (RVar happy_var_1 Nothing
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  18 happyReduction_23
happyReduction_23 (HappyAbsSyn19  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn18
		 ((happy_var_1, happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 18 happyReduction_24
happyReduction_24 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((varName happy_var_2, Term.Fix happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  19 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn19
		 (Term.Var happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  19 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Term.App happy_var_1 (Term.Var happy_var_2)
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 19 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Term.App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  19 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 19 happyReduction_29
happyReduction_29 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Term.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 19 happyReduction_30
happyReduction_30 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Term.Fix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 19 happyReduction_31
happyReduction_31 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Term.Cse empty mempty happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 4 20 happyReduction_32
happyReduction_32 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (RProp happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_3  21 happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Clause.Clause (Clause.flatten happy_var_1) happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  21 happyReduction_34
happyReduction_34 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (Clause.Clause [] happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  22 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn22
		 (Clause.Equal happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenName happy_dollar_dollar -> cont 23;
	TokenBar -> cont 24;
	TokenType -> cont 25;
	TokenArr -> cont 26;
	TokenOP -> cont 27;
	TokenCP -> cont 28;
	TokenEq -> cont 29;
	TokenCase -> cont 30;
	TokenOf -> cont 31;
	TokenLambda -> cont 32;
	TokenFix -> cont 33;
	TokenLet -> cont 34;
	TokenRec -> cont 35;
	TokenIn -> cont 36;
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

binding tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

prop tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

term tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term RVar
type RAlt = Term.Alt RVar
type RClause = Clause.Clause RVar
type REquation = Clause.Equation RVar

data RProp
  = RProp     { propName :: String,
                propVars :: [RVar],
                propClause :: RClause }

data RVar 
  = RVar      { varName :: String,
                varType :: Maybe RType }
  deriving ( Eq, Ord, Show )

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
  | TokenFix
  | TokenLet
  | TokenRec
  | TokenIn
  deriving Show
  
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_"
  
lexer :: String -> [Token]
lexer [] = []
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
