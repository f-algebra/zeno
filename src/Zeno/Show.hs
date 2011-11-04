module Zeno.Show (
  showTyped
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Core

import qualified Data.Set as Set
import qualified Data.Map as Map

instance Show a => Show (Type a) where
  show (VarType var) = show var
  show (FunType arg res) = 
    show_arg ++ " -> " ++ show res
    where 
    show_arg = 
      case arg of 
        FunType _ _ -> "(" ++ show arg ++ ")"
        _ -> show arg

instance Show (DataType a) where
  show = dataTypeName
  
instance Show a => Show (Equality a) where
  show (l :=: r) = show l ++ " = " ++ show r

instance Show a => Show (Clause a) where
  show (Clause vs t cs) = vs_s ++ cs_s ++ show t
    where
    cs_s  | null cs = ""
          | otherwise = (intercalate ", " $ map show cs) ++ " ==> "
    vs_s  | Set.null vs = ""
          | otherwise = "all " ++ (intercalate " " $ map show $ toList vs) ++ " . "
              
instance Show ZVar where
  show (varName -> Just name) = name
  show var = "_" ++ show (varId var)

instance Show a => Show (Term a) where
  show = flip runReader 0 . showTerm
    where
    showAlt :: Show a => Alt a -> Indented String
    showAlt (Alt con binds rhs) = do
      i <- indentation
      rhs_s <- indent $ showTerm rhs
      let con_s = show con ++ concatMap ((" " ++) . show) binds
      return $ i ++ con_s ++ " -> " ++ rhs_s
    
    showTerm :: Show a => Term a -> Indented String
    showTerm (Var var) = (return . stripModuleName . show) var
    showTerm (App lhs rhs) = do
      lhs' <- (indent . showTerm) lhs
      rhs' <- (indent . showTerm) rhs
      let lhs_s = if isVar lhs || isApp lhs then lhs' else "(" ++ lhs' ++ ")"
          rhs_s = if isVar rhs then rhs' else "(" ++ rhs' ++ ")"
      return $ lhs_s ++ " " ++ rhs_s 
    showTerm expr@(Lam {}) = do
      let (vars, rhs) = flattenLam expr
          vars_s = intercalate " " (map show vars)
      rhs_s <- showTerm rhs
      return $ "\\" ++ vars_s ++ " -> " ++ rhs_s
    showTerm (Fix f e) = do
      e' <- showTerm e
      return $ "fix " ++ show f ++ " in " ++ e'
    showTerm (Cse id lhs alts) = do
      i <- indentation
      alts' <- indent . concatMapM showAlt $ alts
      lhs' <- indent . showTerm $ lhs
      let lhs'' | isVar lhs = lhs'
                | otherwise = "(" ++ lhs' ++ ")"
      return $ i ++ "case " ++ lhs'' ++ " of" ++ alts'
    
instance Show ZTheory where
  show thy = dtypes ++ defs
    where
    dtypes = theoryDataTypes thy 
      |> Map.elems 
      |> concatMap showDataType
      
    defs = theoryDefinitions thy 
      |> Map.toList 
      |> filter (not . isVar . snd) 
      |> concatMap showLet
    
    showLet (name, def) = 
      "\nlet " ++ name ++ " = " ++ show def ++ "\n"
    
    showDataType (DataType _ name cons) =
      "\ntype " ++ name ++ " where" 
      ++ concatMap (("\n  " ++) . showTyped) cons ++ "\n"
  
showTyped :: (Show a, Typed a, Show (Type (SimpleType a))) => a -> String
showTyped x = show x ++ " : " ++ show (typeOf x)
