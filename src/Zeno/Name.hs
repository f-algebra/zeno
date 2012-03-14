module Zeno.Name (
  Name, MonadUnique,
  new, clone, invent, declare, label
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
  
new :: MonadUnique m => Maybe String -> m Name
new mby_label = do
  uni <- Unique.new
  let label = maybe (show uni) id mby_label
  return (Name uni label)
  
instance Empty Name where
  empty = Name mempty "NULL"

