module Zeno.Testing (
  run, term, loadPrelude, newVar,
  label, list, assert, assertAlphaEq
) where

import Prelude ()
import Zeno.Prelude hiding ( assert )
import Zeno.Var ( ZTerm, ZVar )
import Zeno.Core ( Zeno )
import Zeno.Show ()
import Zeno.Unification

import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Unique as Unique
import qualified Zeno.Parsing.ZML as ZML
import qualified Zeno.Logic as Logic
import qualified Test.HUnit as HUnit

run :: HUnit.Testable t => Zeno t -> HUnit.Test 
run = HUnit.test . flip evalState empty

term :: String -> Zeno ZTerm
term = ZML.readTerm

list :: [HUnit.Test] -> HUnit.Test
list = HUnit.TestList

label :: String -> HUnit.Test -> HUnit.Test
label = HUnit.TestLabel

assert :: HUnit.Assertable t => t -> HUnit.Test
assert = HUnit.TestCase . HUnit.assert

assertAlphaEq :: ZTerm -> ZTerm -> HUnit.Test
assertAlphaEq x y = 
  label (show (Logic.Equal x y))
  $ assert (x `alphaEq` y)

newVar :: String -> String -> Zeno ZVar
newVar name_s typ_s = do
  typ <- ZML.readType typ_s
  new_var <- Var.declare name_s typ Var.Universal
  Zeno.defineTerm name_s (Term.Var new_var)
  return new_var
  
-- Move this into the counter example finding module
-- it's a assertion that will dynamically check equality
assertEq :: ZTerm -> ZTerm -> a -> a
assertEq t1 t2 x = x
 
loadPrelude :: Zeno ()
loadPrelude = do
  mapM_ process (ZML.readLines prelude)
  where
  process ("let", arg) = ZML.readBinding arg
  process ("type", arg) = ZML.readTypeDef arg
 
  prelude :: String
  prelude = unlines
    [ "type bool = true | false;"
    , "type nat = 0 | succ nat;"
    , "type list = nil | cons nat list;"
    
    , "let rec (add : nat -> nat -> nat) = "
    , "  fun (x : nat) (y : nat) -> "
    , "    case x of"
    , "      0 -> y"
    , "    | succ x' -> succ (add x' y);"
    
    , "let rec (mul : nat -> nat -> nat) =" 
    , "  fun (x : nat) (y : nat) ->"
    , "    case x of"
    , "      0 -> 0"
    , "    | succ x' -> add y (mul x' y);"
   
    , "let rec (app : list -> list -> list) =" 
    , "  fun (xs : list) (ys : list) -> "
    , "    case xs of"
    , "      nil -> ys"
    , "    | cons z zs -> cons z (app zs ys);"
    
    , "let rec (rev : list -> list) = "
    , "  fun (xs : list) ->"
    , "    case xs of"
    , "      nil -> nil"
    , "    | cons y ys -> app (rev ys) (cons y nil);" 
    
    , "let rec (map : (nat -> nat) -> list -> list) ="
    , "  fun (f : nat -> nat) (xs : list) ->"
    , "    case xs of"
    , "      nil -> nil"
    , "    | cons y ys -> cons (f y) (map f ys);"  
    
    , "let rec (filter : (nat -> bool) -> list -> list) ="
    , "  fun (p : nat -> bool) (xs : list) ->"
    , "    case xs of"
    , "      nil -> nil"
    , "    | cons y ys -> "
    , "        case (p y) of"
    , "          true -> cons y (filter p ys)"
    , "        | false -> filter p ys;"

    ]

