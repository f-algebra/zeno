{-# OPTIONS_GHC -w #-}
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), 
  RType, RTerm, RAlt,
  isNameChar, parseTypeDef, parseBinding
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
	| HappyAbsSyn5 (RType)
	| HappyAbsSyn6 ([RType])
	| HappyAbsSyn7 (RTypeDef)
	| HappyAbsSyn8 ((String, [RType]))
	| HappyAbsSyn9 ([(String, [RType])])
	| HappyAbsSyn10 ([RVar])
	| HappyAbsSyn11 ([RAlt])
	| HappyAbsSyn12 (RAlt)
	| HappyAbsSyn13 (RVar)
	| HappyAbsSyn15 ((String, RTerm))
	| HappyAbsSyn16 (RTerm)

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
 action_59 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_2,
 happyReduce_3,
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
 happyReduce_27 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (17) = happyShift action_8
action_0 (7) = happyGoto action_7
action_0 _ = happyFail

action_1 (17) = happyShift action_5
action_1 (29) = happyShift action_6
action_1 (15) = happyGoto action_4
action_1 _ = happyFail

action_2 (17) = happyShift action_3
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (31) = happyAccept
action_4 _ = happyFail

action_5 (23) = happyShift action_12
action_5 _ = happyFail

action_6 (21) = happyShift action_11
action_6 (13) = happyGoto action_10
action_6 _ = happyFail

action_7 (31) = happyAccept
action_7 _ = happyFail

action_8 (23) = happyShift action_9
action_8 _ = happyFail

action_9 (17) = happyShift action_24
action_9 (8) = happyGoto action_22
action_9 (9) = happyGoto action_23
action_9 _ = happyFail

action_10 (23) = happyShift action_21
action_10 _ = happyFail

action_11 (17) = happyShift action_20
action_11 _ = happyFail

action_12 (17) = happyShift action_15
action_12 (21) = happyShift action_16
action_12 (24) = happyShift action_17
action_12 (26) = happyShift action_18
action_12 (27) = happyShift action_19
action_12 (14) = happyGoto action_13
action_12 (16) = happyGoto action_14
action_12 _ = happyFail

action_13 (17) = happyShift action_15
action_13 (21) = happyShift action_16
action_13 (24) = happyShift action_17
action_13 (26) = happyShift action_18
action_13 (27) = happyShift action_19
action_13 (14) = happyGoto action_13
action_13 (16) = happyGoto action_35
action_13 _ = happyReduce_27

action_14 _ = happyReduce_19

action_15 _ = happyReduce_18

action_16 (17) = happyShift action_15
action_16 (21) = happyShift action_16
action_16 (24) = happyShift action_17
action_16 (26) = happyShift action_18
action_16 (27) = happyShift action_19
action_16 (14) = happyGoto action_13
action_16 (16) = happyGoto action_34
action_16 _ = happyFail

action_17 (17) = happyShift action_15
action_17 (21) = happyShift action_16
action_17 (24) = happyShift action_17
action_17 (26) = happyShift action_18
action_17 (27) = happyShift action_19
action_17 (14) = happyGoto action_13
action_17 (16) = happyGoto action_33
action_17 _ = happyFail

action_18 (21) = happyShift action_11
action_18 (13) = happyGoto action_32
action_18 _ = happyFail

action_19 (21) = happyShift action_11
action_19 (13) = happyGoto action_31
action_19 _ = happyFail

action_20 (19) = happyShift action_30
action_20 _ = happyFail

action_21 (17) = happyShift action_15
action_21 (21) = happyShift action_16
action_21 (24) = happyShift action_17
action_21 (26) = happyShift action_18
action_21 (27) = happyShift action_19
action_21 (14) = happyGoto action_13
action_21 (16) = happyGoto action_29
action_21 _ = happyFail

action_22 _ = happyReduce_11

action_23 (18) = happyShift action_28
action_23 _ = happyReduce_7

action_24 (17) = happyShift action_3
action_24 (21) = happyShift action_27
action_24 (5) = happyGoto action_25
action_24 (6) = happyGoto action_26
action_24 _ = happyReduce_9

action_25 (20) = happyShift action_44
action_25 _ = happyReduce_6

action_26 (17) = happyShift action_3
action_26 (21) = happyShift action_27
action_26 (5) = happyGoto action_43
action_26 _ = happyReduce_8

action_27 (17) = happyShift action_3
action_27 (21) = happyShift action_27
action_27 (5) = happyGoto action_42
action_27 _ = happyFail

action_28 (17) = happyShift action_24
action_28 (8) = happyGoto action_41
action_28 _ = happyFail

action_29 _ = happyReduce_20

action_30 (17) = happyShift action_3
action_30 (21) = happyShift action_27
action_30 (5) = happyGoto action_40
action_30 _ = happyFail

action_31 (30) = happyShift action_39
action_31 _ = happyFail

action_32 (20) = happyShift action_38
action_32 _ = happyFail

action_33 (25) = happyShift action_37
action_33 _ = happyFail

action_34 (22) = happyShift action_36
action_34 _ = happyFail

action_35 _ = happyReduce_24

action_36 (17) = happyShift action_15
action_36 (21) = happyShift action_16
action_36 (24) = happyShift action_17
action_36 (26) = happyShift action_18
action_36 (27) = happyShift action_19
action_36 (14) = happyGoto action_13
action_36 (16) = happyGoto action_54
action_36 _ = happyReduce_26

action_37 (17) = happyShift action_15
action_37 (10) = happyGoto action_50
action_37 (11) = happyGoto action_51
action_37 (12) = happyGoto action_52
action_37 (14) = happyGoto action_53
action_37 _ = happyFail

action_38 (17) = happyShift action_15
action_38 (21) = happyShift action_16
action_38 (24) = happyShift action_17
action_38 (26) = happyShift action_18
action_38 (27) = happyShift action_19
action_38 (14) = happyGoto action_13
action_38 (16) = happyGoto action_49
action_38 _ = happyFail

action_39 (17) = happyShift action_15
action_39 (21) = happyShift action_16
action_39 (24) = happyShift action_17
action_39 (26) = happyShift action_18
action_39 (27) = happyShift action_19
action_39 (14) = happyGoto action_13
action_39 (16) = happyGoto action_48
action_39 _ = happyFail

action_40 (20) = happyShift action_44
action_40 (22) = happyShift action_47
action_40 _ = happyFail

action_41 _ = happyReduce_10

action_42 (20) = happyShift action_44
action_42 (22) = happyShift action_46
action_42 _ = happyFail

action_43 (20) = happyShift action_44
action_43 _ = happyReduce_5

action_44 (17) = happyShift action_3
action_44 (21) = happyShift action_27
action_44 (5) = happyGoto action_45
action_44 _ = happyFail

action_45 (20) = happyShift action_44
action_45 _ = happyReduce_3

action_46 _ = happyReduce_4

action_47 _ = happyReduce_17

action_48 _ = happyReduce_22

action_49 _ = happyReduce_21

action_50 (17) = happyShift action_15
action_50 (20) = happyShift action_57
action_50 (14) = happyGoto action_56
action_50 _ = happyFail

action_51 (18) = happyShift action_55
action_51 _ = happyReduce_23

action_52 _ = happyReduce_15

action_53 _ = happyReduce_13

action_54 _ = happyReduce_25

action_55 (17) = happyShift action_15
action_55 (10) = happyGoto action_50
action_55 (12) = happyGoto action_59
action_55 (14) = happyGoto action_53
action_55 _ = happyFail

action_56 _ = happyReduce_12

action_57 (17) = happyShift action_15
action_57 (21) = happyShift action_16
action_57 (24) = happyShift action_17
action_57 (26) = happyShift action_18
action_57 (27) = happyShift action_19
action_57 (14) = happyGoto action_13
action_57 (16) = happyGoto action_58
action_57 _ = happyFail

action_58 _ = happyReduce_16

action_59 _ = happyReduce_14

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Type.Var happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Type.Fun happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (RTypeDef happy_var_1
                                          (map (mkTypeDefCon happy_var_1) happy_var_3)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 ((happy_var_1, happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 ((happy_var_1, [])
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 (Term.Alt (head happy_var_1) (tail happy_var_1) happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 5 13 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (RVar happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn13
		 (RVar happy_var_1 Nothing
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn15
		 ((happy_var_1, happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 15 happyReduction_20
happyReduction_20 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((varName happy_var_2, Term.Fix happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 16 happyReduction_21
happyReduction_21 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Term.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 16 happyReduction_22
happyReduction_22 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Term.Fix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 16 happyReduction_23
happyReduction_23 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Term.Cse Name.empty mempty happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2  16 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn16
		 (Term.App (Term.Var happy_var_1) happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 16 happyReduction_25
happyReduction_25 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Term.App happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn16
		 (Term.Var happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenVar happy_dollar_dollar -> cont 17;
	TokenBar -> cont 18;
	TokenType -> cont 19;
	TokenArr -> cont 20;
	TokenOP -> cont 21;
	TokenCP -> cont 22;
	TokenEq -> cont 23;
	TokenCase -> cont 24;
	TokenOf -> cont 25;
	TokenLambda -> cont 26;
	TokenFix -> cont 27;
	TokenLet -> cont 28;
	TokenRec -> cont 29;
	TokenIn -> cont 30;
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

binding tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term RVar
type RAlt = Term.Alt RVar

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
  | TokenVar String
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
isNameChar c = isAlpha c || c `elem` "'"
  
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
      (var, rest) -> TokenVar var : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

parseTypeDef :: String -> RTypeDef
parseTypeDef = typeDef . lexer

parseBinding :: String -> (String, RTerm)
parseBinding = binding . lexer

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
