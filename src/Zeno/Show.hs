module Show (
  showTypeDef, showTyped
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Core

instance Show a => Show (Type a) where
  show (Type tvars inner) 
    | Set.null tvars = show inner
    | otherwise = "forall " ++ vars_s ++ " . " ++ show inner ++ ")"
    where vars_s = (intercalate " " . map show . toList) tvars

instance Show a => Show (InnerType a) where
  show (VarType var) = show var
  show (AppType fun args) = show fun ++ " " ++ args_s
    where args_s = (intercalate " " . map show) args
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
  show (Clause t cs) = 
    let cs_s = case cs of
          [] -> ""
          cs -> cs |> toList |> map show |> intercalate ", " |> (++ " ==> ")
    in cs_s ++ show t
    
instance Show ZVar where
  show var = case varName var of
    Just name -> 
      let name' = stripModuleName name
      in if "$c" `isPrefixOf` name'
        then drop 2 name'
        else name' 
    Nothing -> "_" ++ show (varId var) -- ++ srs
    where
    srs = case allSources var of
      [] -> ""
      srs -> "{" ++ (intercalate "," . map show) srs ++ "}"
  
instance Show a => Show (Term a) where
  show = flip runReader 0 . showTerm            

showAlt :: Show a => Alt a -> Indented String
showAlt (Alt con binds rhs) = do
  i <- indentation
  rhs_s <- indent $ showTerm rhs
  let con_s = case con of
        AltDefault -> "_"
        AltCon var -> show var ++ concatMap ((" " ++) . show) binds
  return $ i ++ con_s ++ " -> " ++ rhs_s

showTerm :: Show a => Term a -> Indented String
showTerm (Var var) = (return . stripModuleName . show) var
showTerm (flattenApp -> Var fun : args) 
  | (show fun == "(,)" && length args == 2)
    || (show fun == "(,,)" && length args == 3)
    || (show fun == "(,,,)" && length args == 4) = do
       args' <- mapM showTerm args
       return $
         "(" ++ intercalate ", " args' ++ ")"
showTerm (App (App (Var fun) arg1) arg2) 
  | isOperator fun_s && isTerm arg1 && isTerm arg2 = do
    arg1' <- (indent . showTerm) arg1
    arg2' <- (indent . showTerm) arg2
    let arg1_s = if isTerm arg1 then arg1' else "(" ++ arg1' ++ ")"
        arg2_s = if isTerm arg2 then arg2' else "(" ++ arg2' ++ ")"
    if fun_s == ":" && arg2_s == "[]" 
      then return $ "[" ++ arg1_s ++ "]"
      else return $ "(" ++ arg1_s ++ " " ++ fun_s ++ " " ++ arg2_s ++ ")"
  where
  fun_s = show fun
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
  where
  flattenLam :: Term a -> ([a], Term a)
  flattenLam (Lam var rhs) = (var : vars, real_rhs)
    where (vars, real_rhs) = flattenLam rhs
  flattenLam expr = ([], expr)
showTerm (Cse id lhs alts) = do
  alts' <- indent . concatMapM showAlt $ alts
  lhs' <- indent . showTerm $ lhs
  let lhs'' = if isTerm lhs then lhs' else "(" ++ lhs' ++ ")"
  i <- indentation
  return $ i ++ "case " ++ lhs'' ++ " of" ++ alts'
  
-- |Use when you want the full description of a data-type, as 'show' will just
-- output its name.
showTypeDef :: (Show a, Typed a, Show (Type (SimpleType a))) => 
  DataType a -> String
showTypeDef (DataType _ name cons) =
  "\ntype " ++ name ++
  " where" ++ concatMap (("\n  " ++) . showTyped) cons ++ "\n"
  
showTyped :: (Show a, Typed a, Show (Type (SimpleType a))) => a -> String
showTyped x = "(" ++ show x ++ " " ++ show (typeOf x) ++ ")"
