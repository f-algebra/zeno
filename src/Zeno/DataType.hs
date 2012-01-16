module Zeno.DataType (
  DataType (..),
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Name ( Name )

data DataType a
  = DataType    { name :: !Name,
                  constructors :: ![a] }

instance Empty (DataType a) where
  empty = DataType empty mempty
                  
instance Eq (DataType a) where
  (==) = (==) `on` name
 
instance Ord (DataType a) where
  compare = compare `on` name

