module Zeno.Show (
  showTyped
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Type ( Type, Typed (..) )
import Zeno.DataType ( DataType )
import Zeno.Core ( ZenoTheory )
import Zeno.Var ( ZVar )
import Zeno.Term ( Term, Alt )
import Zeno.Clause ( Equation, Clause )

import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Core as Zeno

import qualified Data.Set as Set
import qualified Data.Map as Map

instance Show a => Show (Type a) where
  show (Type.Var var) = show var
  show (Type.Fun arg res) = 
    show_arg ++ " -> " ++ show res
    where 
    show_arg = 
      case arg of 
        Type.Fun _ _ -> "(" ++ show arg ++ ")"
        _ -> show arg

instance Show (DataType a) where
  show = show . DataType.name
  
instance Show a => Show (Equation a) where
  show (Clause.Equal l r) = show l ++ " = " ++ show r

instance Show a => Show (Clause a) where
  show cls = ants_s ++ (show . Clause.consequent) cls
    where
    ants = Clause.antecedents cls
    ants_s  | null ants = ""
            | otherwise = (intercalate ", " $ map show ants) ++ " ==> "
              
instance Show ZVar where
  show = show . Var.name 

instance Show a => Show (Term a) where
  show = flip runReader 0 . showTerm
    where
    showAlt :: Show a => Alt a -> Indented String
    showAlt (Term.Alt con binds rhs) = do
      i <- indentation
      rhs_s <- indent $ showTerm rhs
      let con_s = show con ++ concatMap ((" " ++) . show) binds
      return $ i ++ con_s ++ " -> " ++ rhs_s
    
    showTerm :: Show a => Term a -> Indented String
    showTerm (Term.Var var) = (return . stripModuleName . show) var
    showTerm (Term.App lhs rhs) = do
      lhs' <- (indent . showTerm) lhs
      rhs' <- (indent . showTerm) rhs
      let lhs_s | Term.isVar lhs || Term.isApp lhs = lhs'
                | otherwise = "(" ++ lhs' ++ ")"
          rhs_s | Term.isVar rhs = rhs' 
                | otherwise = "(" ++ rhs' ++ ")"
      return $ lhs_s ++ " " ++ rhs_s 
    showTerm expr@(Term.Lam {}) = do
      let (vars, rhs) = Term.flattenLam expr
          vars_s = intercalate " " (map show vars)
      rhs_s <- showTerm rhs
      return $ "fun " ++ vars_s ++ " -> " ++ rhs_s
    showTerm (Term.Fix f e) = do
      e' <- showTerm e
      return $ "fix " ++ show f ++ " in " ++ e'
    showTerm (Term.Cse lbl _ lhs alts) = do
      i <- indentation
      alts' <- indent . concatMapM showAlt $ alts
      lhs' <- indent . showTerm $ lhs
      let lhs'' | Term.isVar lhs = lhs'
                | otherwise = "(" ++ lhs' ++ ")"
      return $ i ++ "case " ++ lhs'' ++ " of" ++ alts'
    
instance Show ZenoTheory where
  show thy = dtypes ++ defs ++ conjs
    where
    dtypes = Zeno.dataTypes thy 
      |> Map.elems 
      |> concatMap showDataType
      
    defs = Zeno.definitions thy 
      |> Map.toList 
      |> filter (not . Term.isVar . snd) 
      |> concatMap showLet
      
    conjs = Zeno.conjectures thy
      |> Map.toList
      |> concatMap showProp
      
    showProp (name, cls) = 
      "\nprop " ++ name ++ " = " ++ show cls ++ "\n"
    
    showLet (name, def) = 
      "\nlet " ++ name ++ " = " ++ show def ++ "\n"
    
    showDataType dtype =
      "\ntype " ++ show (DataType.name dtype) ++ " where" 
      ++ concatMap (("\n  " ++) . showTyped) (DataType.cons dtype) ++ "\n"
  
showTyped :: (Show a, Typed a, Show (Type (SimpleType a))) => a -> String
showTyped x = show x ++ " : " ++ show (typeOf x)
