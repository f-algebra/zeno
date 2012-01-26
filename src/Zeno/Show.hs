{-# LANGUAGE UndecidableInstances #-}
module Zeno.Show (
  showTyped, showWithDefinitions, showSubstitution
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Traversing
import Zeno.Type ( Type, Typed (..) )
import Zeno.DataType ( DataType )
import Zeno.Core ( ZenoTheory )
import Zeno.Var ( ZVar, ZTerm, ZAlt )
import Zeno.Term ( Term, Alt )
import Zeno.Logic ( Equation, Clause )

import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Zeno.Logic as Logic
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
  
instance Show (Term a) => Show (Equation a) where
  show (Logic.Equal l r) = show l ++ " = " ++ show r

instance Show (Term a) => Show (Clause a) where
  show = intercalate " ->\n  " . map show . Logic.flatten
              
instance Show ZVar where
  show = show . Var.name 
  
type ShowTerm = Reader (Int, (Map String Int, Map ZVar String))

instance IndentationMonad ShowTerm where
  indent = local (first (+ 1))
  resetIndent = local (first (const 0))
  indentation = asks $ \(i, _) -> fromString $ "\n" ++ (concat . replicate i) "  "

showVar :: ZVar -> ShowTerm String
showVar var = do
  var_map <- asks (snd . snd)
  case Map.lookup var var_map of
    Nothing -> return (show var)
    Just name -> return name
  
bindVar :: ZVar -> ShowTerm b -> ShowTerm b
bindVar var = local $ second $ updateMaps
  where
  name = show var
  
  updateMaps (name_count, var_names) =
    case Map.lookup name name_count of
      Nothing -> (Map.insert name 2 name_count, 
                  Map.insert var name var_names)
                  
      Just cn -> (Map.insert name (cn + 1) name_count,
                  Map.insert var (name ++ show cn) var_names)
  
bindVars :: [ZVar] -> ShowTerm b -> ShowTerm b
bindVars = appEndo . concatMap (Endo . bindVar)
  
instance Show ZTerm where
  show = flip runReader (0, mempty) . showTerm
    where
    showAlt :: ZAlt -> ShowTerm String
    showAlt (Term.Alt con binds rhs) = bindVars binds $ do
      i <- indentation
      rhs_s <- indent $ showTerm rhs
      binds_s <- mapM showVar binds
      let con_s = show con ++ concatMap (" " ++) binds_s
      return $ i ++ "| " ++ con_s ++ " -> " ++ rhs_s
    
    showTerm :: ZTerm -> ShowTerm String
    showTerm (Term.Var var) = showVar var
    showTerm (Term.App lhs rhs) = do
      lhs' <- (indent . showTerm) lhs
      rhs' <- (indent . showTerm) rhs
      let lhs_s | Term.isVar lhs || Term.isApp lhs || Term.isFix lhs = lhs'
                | otherwise = "(" ++ lhs' ++ ")"
          rhs_s | Term.isVar rhs || Term.isFix rhs = rhs' 
                | otherwise = "(" ++ rhs' ++ ")"
      return $ lhs_s ++ " " ++ rhs_s 
    showTerm expr@(Term.Lam {}) = do
      let (vars, rhs) = Term.flattenLam expr
          vars_s = intercalate " " (map show vars)
      rhs_s <- bindVars vars $ showTerm rhs
      return $ "fun " ++ vars_s ++ " -> " ++ rhs_s
    showTerm (Term.Fix f e) = return (show f)
      {- do
      e' <- showTerm e
      return $ "fix " ++ show f ++ " in " ++ e' -}
    showTerm (Term.Cse _ _ lhs alts) = indent $ do
      i <- indentation
      alts' <- concatMapM showAlt $ alts
      lhs' <- indent . showTerm $ lhs
      let lhs'' | Term.isVar lhs = lhs'
                | otherwise = "(" ++ lhs' ++ ")"
      return $ i ++ "case " ++ lhs'' ++ " of" ++ alts'
     
instance Show ZenoTheory where
  show thy = types ++ terms ++ conjs
    where
    types = Zeno.types thy 
      |> Map.elems 
      |> concatMap showType
      
    terms = Zeno.terms thy 
      |> Map.toList 
      |> filter (not . Term.isVar . snd) 
      |> concatMap showTerm
      
    conjs = Zeno.props thy
      |> Map.toList
      |> concatMap showProp
      
    showProp (name, cls) = 
      "\nprop " ++ name ++ " = " ++ show cls ++ "\n"
    
    showTerm (name, def) = 
      "\nlet " ++ name ++ " = " ++ show def ++ "\n"
    
    showType dtype =
      "\ntype " ++ show (DataType.name dtype) ++ " where" 
      ++ concatMap (("\n  " ++) . showTyped) (DataType.constructors dtype) ++ "\n"
  
showTyped :: (Show a, Typed a, Show (Type (SimpleType a))) => a -> String
showTyped x = show x ++ " : " ++ show (typeOf x)

showWithDefinitions :: 
  (WithinTraversable ZTerm (t ZVar), Show (t ZVar)) => t ZVar -> String
showWithDefinitions has_terms = show has_terms ++ "\nwhere\n" ++ defs
  where
  defs = intercalate "\n"                
       $ map showDef 
       $ Map.toList
       $ foldWithin collectDefs has_terms
  
  collectDefs :: ZTerm -> Map String String
  collectDefs (Term.Fix var def) = Map.singleton (show var) (show def)
  collectDefs other = mempty
  
  showDef (name, def) = name ++ " = " ++ def
  
showSubstitution :: (Show k, Show v) => Substitution k v -> String
showSubstitution (Map.toList -> subs) =
  "[ " ++ (intercalate "; " . map showSub) subs ++ " ]"
  where showSub (k, v) = show k ++ " -> " ++ show v
