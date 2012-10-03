module Zeno.Engine.Inventor (
  transform
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Var ( ZTerm, ZVar )
import Zeno.Type ( typeOf )
import Zeno.Show
 
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.DataType as DataType

import qualified Control.Failure as Fail
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Attempts to discover a function which will transform the first
-- argument into the second argument.
transform :: (MonadUnique m, MonadFailure m) => 
  ZTerm -> ZTerm -> m ZTerm
transform from_term target_term = checkVars $ do
  
  
  
    
  
  
  where
  fold_type@(Type.Var fold_dtype) = typeOf from_term
  
  -- The type of our new function, the argument will be the 
  -- input 'from_term', the result type must obviously match 'target_term'
  func_type = Type.unflatten [fold_type, typeOf target_term]
  
  
  -- | This is a post-condition of the deforestation process
  -- which will call this function
  checkVars = assert 
    $ freeVars from_term `Set.isSubsetOf` free_vars
    
    
-- | Creates the "fold" function for a given datatype. The order of its
-- arguments will match the order of 'DataType.constructors'
makeFold :: MonadUnique m => ZDataType -> ZType -> m ZTerm
makeFold input_dtype result_type = do
  -- Create a new variable for the new function we are inventing
  fun_var <- Var.invent func_type
  
  -- Create a new variable for every constructor of the input type,
  -- these variables will define the recursion/fold over that type
  fold_kvars <- mapM Var.invent arg_types
    
  
  where
  arg_types = 
    map (replaceWithin (Type.Var input_dtype) result_type)
    $ map typeOf 
    $ DataType.constructors fold_dtype
