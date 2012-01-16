module Zeno.Name (
  Unique, Name, UniqueGen (..),
  new, clone, invent, declare, label
) where

import Prelude ()
import Zeno.Prelude

newtype Unique = Unique { runUnique :: Int }
  deriving ( Eq, Ord )

data Name = Name  { nameId :: !Unique,
                    label :: !String }

class UniqueGen g where
  takeUnique :: g -> (Unique, g)

instance Eq Name where
  (==) = (==) `on` nameId
  
instance Ord Name where
  compare = compare `on` nameId

instance UniqueGen Unique where
  takeUnique (Unique i) = (uni', uni')
    where uni' = Unique (i + 1)

instance Show Unique where
  show = intToChars . runUnique
    where
    intToChars :: Int -> String
    intToChars 0 = []                          
    intToChars n = 
      let c = chr $ (n `mod` 26) + (ord 'a')
      in c : intToChars (n `div` 26)

instance Show Name where
  show = label

instance Monoid Unique where
  mempty = Unique 0
  mappend (Unique i) (Unique j) = Unique (max i j)

unique :: (UniqueGen g, MonadState g m) => m Unique
unique = do
  g <- get
  let (uni, new_g) = takeUnique g
  put new_g
  return uni
  
clone :: (UniqueGen g, MonadState g m) => Name -> m Name
clone = declare . label

invent :: (UniqueGen g, MonadState g m) => m Name
invent = new Nothing

declare :: (UniqueGen g, MonadState g m) => String -> m Name
declare = new . Just
  
new :: (UniqueGen g, MonadState g m) => Maybe String -> m Name
new mby_label = do
  uni <- unique
  let label = maybe (show uni) id mby_label
  return (Name uni label)
  
instance Empty Name where
  empty = Name mempty "NULL"

