module Zeno.Id (
  Id, IdCounter (..), 
  firstId, reservedId, nextId, newIdS
) where

import Prelude ()
import Zeno.Prelude

newtype Id = Id { runId :: Int }
  deriving ( Eq, Ord )
  
class IdCounter a where
  newId :: a -> (Id, a)
  largestId :: a -> Id
  
instance IdCounter (a, Id) where
  newId (a, id) = (id', (a, id'))
    where id' = nextId id
  largestId = snd
  
instance IdCounter Id where
  newId id = (nextId id, nextId id)
  largestId = id 
 
instance Show Id where
  show = idToChars
  
instance Monoid Id where
  mempty = firstId
  mappend = error "Why would you do this?"
  
idToChars :: Id -> String
idToChars = intToChars . runId
  where
  intToChars :: Int -> String
  intToChars 0 = []                          
  intToChars n = 
    let c = chr $ (n `mod` 26) + (ord 'a')
    in c : intToChars (n `div` 26)  

newIdS :: (IdCounter s, MonadState s m) => m Id
newIdS = do
  a <- get
  let (id, new_a) = newId a
  put new_a
  return id
  
firstId :: Id
firstId = nextId reservedId

reservedId :: Id
reservedId = Id 1

nextId :: Id -> Id
nextId (Id id) = Id (id + 1)
