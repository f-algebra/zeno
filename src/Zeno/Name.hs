module Zeno.Name (
  Name, MonadUnique, Unique, 
  Has (..),
  new, invent, declare, label, relabel,
  global, unsafe,
  uniqueId,
) where

import Prelude ()
import Zeno.Prelude hiding ( get )
import Zeno.Unique ( Unique, MonadUnique )
import qualified Zeno.Unique as Unique

data Name = Name  { uniqueId :: !Unique,
                    label :: !String }

class Has a where
  get :: a -> Name
  freshen :: MonadUnique m => a -> m a

instance Eq Name where
  (==) = (==) `on` uniqueId
  
instance Ord Name where
  compare = compare `on` uniqueId

instance Show Name where
  show = label
  
instance Has Name where
  get = id
  freshen = declare . label

invent :: MonadUnique m => m Name
invent = new Nothing

declare :: MonadUnique m => String -> m Name
declare = new . Just

global :: Maybe String -> IO Name
global mby_lbl = do
  uni <- Unique.global
  let lbl = maybe ("?" ++ show uni) id mby_lbl
  return (Name uni lbl)

unsafe :: String -> Name
unsafe = unsafePerformIO . global . Just

new :: MonadUnique m => Maybe String -> m Name
new mby_label = do
  uni <- Unique.new
  let label = maybe ("_" ++ show uni) id mby_label
  return (Name uni label)
  
relabel :: String -> Name -> Name
relabel lbl (Name id _) = Name id lbl
  
instance Empty Name where
  empty = Name mempty "NULL"

