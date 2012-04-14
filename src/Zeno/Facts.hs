module Zeno.Facts (
  Reader (..), State (..)
) where

import Prelude ()
import Zeno.Prelude hiding ( ask, Reader, State )
import Zeno.Var ( ZVar, ZTerm, ZEquation )

import qualified Control.Monad.Reader as Reader

class Monad m => Reader m where
  ask :: m [ZEquation]
  add :: ZEquation -> m a -> m a  
  
class Reader m => State m where
  set :: [ZEquation] -> m ()
    
instance Reader Identity where
  ask = return []
  add _ = id

