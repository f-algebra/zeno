module Zeno.DataType (
  DataType (..),
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Id
import Zeno.Type

data DataType a
  = DataType    { dataTypeId :: !Id,
                  dataTypeName :: !String,
                  dataTypeCons :: ![a] }
  deriving ( Functor, Foldable, Traversable ) 


instance Eq (DataType a) where
  (==) = (==) `on` dataTypeId
  
instance Ord (DataType a) where
  compare = compare `on` dataTypeId

