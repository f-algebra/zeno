module Zeno.Name (
  Name, MonadUnique, Unique,
  new, clone, invent, declare, label, relabel,
  global, unsafe
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Unique ( Unique, MonadUnique )
import qualified Zeno.Unique as Unique

data Name = Name  { nameId :: !Unique,
                    label :: !String }

instance Eq Name where
  (==) = (==) `on` nameId
  
instance Ord Name where
  compare = compare `on` nameId

instance Show Name where
  show = label
  
clone :: MonadUnique m => Name -> m Name
clone = declare . label

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
  let label = maybe ("?" ++ show uni) id mby_label
  return (Name uni label)
  
relabel :: String -> Name -> Name
relabel lbl (Name id _) = Name id lbl
  
instance Empty Name where
  empty = Name mempty "NULL"

