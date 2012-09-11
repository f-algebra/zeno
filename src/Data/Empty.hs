module Data.Empty (
  Empty (..)
) where

-- | Sometimes you need an 'mempty' element, but don't have an
-- appropriate 'mappend', so 'Empty' is a convenient superset of 'Monoid'.
class Empty a where
  empty :: a
  
instance Empty (a -> a) where
  empty = id

