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
import Zeno.Term ( Term, Alt )
import Zeno.Logic ( Equation, Clause )

import qualified Zeno.DataType as DataType
import qualified Zeno.Logic as Logic
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type

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

type ShowTerm a = Reader (Int, (Map String Int, Map a String))

instance IndentationMonad (ShowTerm a) where
  indent = local (first (+ 1))
  resetIndent = local (first (const 0))
  indentation = asks $ \(i, _) -> fromString $ "\n" ++ (concat . replicate i) "  "

showVar :: (Ord a, Show a) => a -> ShowTerm a String
showVar var = do
  var_map <- asks (snd . snd)
  case Map.lookup var var_map of
    Nothing -> return (show var)
    Just name -> return name
  
bindVar :: (Ord a, Show a) => a -> ShowTerm a b -> ShowTerm a b
bindVar var = local $ second $ updateMaps
  where
  name = show var
  
  updateMaps (name_count, var_names) =
    case Map.lookup name name_count of
      Nothing -> (Map.insert name 2 name_count, 
                  Map.insert var name var_names)
                  
      Just cn -> (Map.insert name (cn + 1) name_count,
                  Map.insert var (name ++ show cn) var_names)
  
bindVars :: (Ord a, Show a) => [a] -> ShowTerm a b -> ShowTerm a b
bindVars = appEndo . concatMap (Endo . bindVar)
  
showAlt :: (Ord a, Show a) => Alt a -> ShowTerm a String
showAlt (Term.Alt con binds rhs) = bindVars binds $ do
  i <- indentation
  rhs_s <- indent $ showTerm rhs
  binds_s <- mapM showVar binds
  let con_s = show con ++ concatMap (" " ++) binds_s
  return $ i ++ "| " ++ con_s ++ " -> " ++ rhs_s
  
showTerm :: (Ord a, Show a) => Term a -> ShowTerm a String
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
showTerm (Term.Fix f e) =  return (show f) {-
  do
    e' <- showTerm e
    return $ "(fix " ++ show f ++ " in " ++ e' ++ ")" -}
showTerm (Term.Cse srt lhs alts) = indent $ do
  i <- indentation
  alts' <- concatMapM showAlt $ alts
  lhs' <- indent . showTerm $ lhs
  let lhs'' | Term.isVar lhs = lhs'
            | otherwise = "(" ++ lhs' ++ ")"
      srt_s | Term.isFoldCase srt = ""
            | otherwise = "?"
  return $ i ++ srt_s ++ "case " ++ lhs'' ++ " of" ++ alts'
  
instance (Ord a, Show a) => Show (Alt a) where
  show = flip runReader (0, mempty) . showAlt

instance (Ord a, Show a) => Show (Term a) where
  show = flip runReader (0, mempty) . showTerm
    
showTyped :: (Show a, Typed a, Show (Type (SimpleType a))) => a -> String
showTyped x = show x ++ " : " ++ show (typeOf x)

showWithDefinitions :: forall a t .
  (WithinTraversable (Term a) (t a), Show (t a), Ord a, Show a) => t a -> String
showWithDefinitions has_terms = show has_terms ++ defs_s
  where
  defs_s | null defs = ""
         | otherwise = "\nwhere\n" ++ intercalate "\n" defs
  
  defs = map showDef 
       $ Map.toList
       $ foldWithin collectDefs has_terms
  
  collectDefs :: Term a -> Map String String
  collectDefs (Term.Fix var def) = Map.singleton (show var) (show def)
  collectDefs other = mempty
  
  showDef (name, def) = name ++ " = " ++ def
  
showSubstitution :: (Show k, Show v) => Substitution k v -> String
showSubstitution (Map.toList -> subs) =
  "[ " ++ (intercalate "; " . map showSub) subs ++ " ]"
  where showSub (k, v) = show k ++ " -> " ++ show v
