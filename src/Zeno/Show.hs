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

type ShowTerm a = ReaderT Int (State (Map String Int, Map a String))

runShowTerm :: Ord a => ShowTerm a b -> b
runShowTerm = flip evalState mempty . flip runReaderT 0 

instance IndentationMonad (ShowTerm a) where
  indent = local (+ 1)
  resetIndent = local (const 0)
  indentation = asks $ \i -> fromString $ "\n" ++ (concat . replicate i) "  "

showVar :: (Ord a, Show a) => a -> ShowTerm a String
showVar var = do
  var_map <- gets snd
  case Map.lookup var var_map of
    Just name -> return name
    Nothing -> do
      i_map <- gets fst
      case Map.lookup var_s i_map of
        Nothing -> do
          modify $ Map.insert var_s 2 *** Map.insert var var_s
          return var_s
        Just i -> do
          let new_name = var_s ++ show i
          modify $ Map.insert var_s (i + 1) *** Map.insert var new_name
          return new_name
  where
  var_s = show var
  {-
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
  -}
showAlt :: (Ord a, Show a) => Alt a -> ShowTerm a String
showAlt (Term.Alt con binds rhs) = do
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
  vars_s <- intercalate " " <$> mapM showVar vars
  rhs_s <- showTerm rhs
  return $ "fun " ++ vars_s ++ " -> " ++ rhs_s
showTerm (Term.Fix f e) = showVar f {-
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
  
instance Show a => Show (Term.CaseSort a) where
  show Term.SplitCase = "<>"
  show (Term.FoldCase name fix) = "<" ++ show name ++ ", " ++ show fix ++ ">"
  
instance (Ord a, Show a) => Show (Alt a) where
  show = runShowTerm . showAlt

instance (Ord a, Show a) => Show (Term a) where
  show = runShowTerm . showTerm
    
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
