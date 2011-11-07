-- |A load of random stuff from various Haskell projects, 
-- most of this is unused by Zeno.
module Zeno.Utils where

import Prelude ()
import Zeno.Prelude

import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import System.Random

import qualified Data.Map as Map
import qualified Data.Set as Set

infixl 0 |> 

(|>) :: a -> (a -> b) -> b
x |> f = f x

traceMe :: Show a => String -> a -> a
traceMe s x = trace (s ++ ": " ++ show x) x
  
orderedSupersetOf :: Eq a => [a] -> [a] -> Bool
orderedSupersetOf _ [] = True
orderedSupersetOf [] _ = False
orderedSupersetOf (x:xs) (y:ys)
  | x == y = xs `orderedSupersetOf` ys
  | otherwise = xs `orderedSupersetOf` (y:ys)

nubAndDelete :: Ord a => a -> [a] -> [a]
nubAndDelete n = Set.toList . Set.delete n . Set.fromList 

nubAndDifference :: Ord a => [a] -> [a] -> [a]
nubAndDifference xs ys = Set.toList $ 
  Set.difference (Set.fromList xs) (Set.fromList ys)
  
newtype EndoKleisli m a b = 
  EndoKleisli { runEndoKleisli :: (a -> m a, b) }

instance (Monad m, Monoid b) => Monoid (EndoKleisli m a b) where
  mempty = EndoKleisli (return, mempty)
  (EndoKleisli (f, a)) `mappend` (EndoKleisli (g, b)) = 
    EndoKleisli (f >=> g, a `mappend` b)
  
instance Monad m => Monad (EndoKleisli m a) where
  return x = EndoKleisli (return, x)
  (EndoKleisli (f, a)) >>= g = 
    let EndoKleisli (h, b) = g a in EndoKleisli (f >=> h, b)
  
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = f *** f
  
mapPairM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
mapPairM f (x, y) = do
  x' <- f x
  y' <- f y
  return (x', y')
    
mapM_to_fmap :: 
  ((a -> Identity b) -> c -> Identity d) -> (a -> b) -> c -> d
{-# INLINE mapM_to_fmap #-}
mapM_to_fmap mf f = mf (f >>> Identity) >>> runIdentity

mapM_to_mapMaybe :: 
  ((a -> Writer Any b) -> c -> Writer Any d) -> (a -> Maybe b) -> c -> Maybe d
{-# INLINE mapM_to_mapMaybe #-}
mapM_to_mapMaybe mf f c =
  let (d, any) = runWriter (mf f' c)
  in if getAny any then Nothing else Just d
  where 
  f' x = case f x of 
    Nothing -> tell (Any True) >> return undefined
    Just x' -> return x'

mapM_to_foldMap :: (Monoid m) => 
  ((a -> Writer m a) -> b -> Writer m c) -> (a -> m) -> b -> m
mapM_to_foldMap mf f = mf f' >>> execWriter
  where f' x = tell (f x) >> return x

mapM_to_foldMapA :: (Monoid m, Applicative t) => 
  ((a -> Writer m (t a)) -> b -> Writer m c) -> (a -> m) -> b -> m
mapM_to_foldMapA mf f = mf f' >>> execWriter
  where f' x = tell (f x) >> return (pure x)
  
mapM_to_foldMapAM :: (Monoid m, Monad f, Applicative t) =>
  ((a -> WriterT m f (t a)) -> b -> WriterT m f c) -> (a -> f m) -> b -> f m
mapM_to_foldMapAM mf f = mf (f_gen f) >>> execWriterT
  where 
    f_gen :: (Monoid m, Monad f, Applicative t) => (a -> f m) -> a -> WriterT m f (t a)
    f_gen f x = do
      x' <- lift $ f x
      tell x'
      return (pure x)
      
mapM_to_foldMapM :: (Monoid m, Monad f) =>
  ((a -> WriterT m f a) -> b -> WriterT m f c) -> (a -> f m) -> b -> f m
mapM_to_foldMapM mf f = mf (f_gen f) >>> execWriterT
  where 
    f_gen :: (Monoid m, Monad f) => 
      (a -> f m) -> a -> WriterT m f a
    f_gen f x = do
      x' <- lift $ f x
      tell x'
      return x
      
foldMap_to_foldr :: ((a -> Endo b) -> c -> Endo b) -> 
  (a -> b -> b) -> b -> c -> b
foldMap_to_foldr foldMap f z t = appEndo (foldMap (Endo . f) t) z

mapM_to_foldrM :: (Monad m, Applicative t) => 
  ((a -> EndoKleisli m b (t a)) -> c -> EndoKleisli m b d) -> 
    (a -> b -> m b) -> b -> c -> m b
mapM_to_foldrM mapM f z t = 
  (fst $ runEndoKleisli $ mapM (fld f) t) z
  where
    fld :: (Monad m, Applicative t) => 
      (a -> b -> m b) -> a -> EndoKleisli m b (t a)
    fld f x = EndoKleisli (f x, pure x)
    
flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

pickEach :: [a] -> [(a, [a])]
pickEach = pickEach' []
  where 
    pickEach' _ [] = []
    pickEach' ys (x : xs) = 
      (x, ys ++ xs) : pickEach' (x : ys) xs
      
filterEach :: ((a, [a]) -> Bool) -> [a] -> [a]
filterEach = filterEach' []
  where
    filterEach' _ _ [] = []
    filterEach' ys p (x : xs) = 
      if p (x, ys ++ xs)
        then x : filterEach' (x : ys) p xs
        else filterEach' ys p xs
  
infixr 9 <.>
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c) 
(<.>) = ((.) . fmap)

infixr 9 <.<
(<.<) :: Monad m => (b -> c) -> (a -> m b) -> (a -> m c) 
(<.<) = ((.) . liftM)

stripModuleName :: String -> String
stripModuleName name = 
  if '.' `elem` name 
    then (stripModuleName . tail . dropWhile (/= '.')) name
    else name

stripEndNumber :: String -> String
stripEndNumber name = 
  if isAlpha $ head name 
    then name
    else takeWhile (not . isDigit) name
    
butlast :: [a] -> [a]
butlast [] = []
butlast [x] = []
butlast (x : xs) = x : (butlast xs)

replace :: (Functor f, Eq a) => a -> a -> f a -> f a
replace = genericReplace fmap

replaceMany :: (Foldable t, Functor f, Eq a) => t (a, a) -> f a -> f a
replaceMany = genericReplaceMany fmap

genericReplace :: Eq a => ((a -> a) -> b -> b) -> a -> a -> b -> b
{-# INLINE genericReplace #-}
genericReplace f x y = f $ \a -> if a == x then y else a

genericReplaceMany :: (Foldable t, Eq a) => 
  ((a -> a) -> b -> b) -> t (a, a) -> b -> b
{-# INLINE genericReplaceMany #-}
genericReplaceMany f = f . repl . toList
  where 
  repl rs a = 
    case lookup a rs of
      Nothing -> a
      Just r -> r

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = (mapM f) >>> (liftM (filter isJust >>> map fromJust))

containsDuplicates :: (Foldable t, Ord a) => t a -> Bool
containsDuplicates = toList >>> (\x -> length (nubOrd x) /= length x)
      
clusterBy :: (a -> a -> Bool) -> [a] -> [[a]]
clusterBy _ [] = []
clusterBy f (a : as) = (a : as') : (clusterBy f as'')
  where (as', as'') = partition (f a) as
  
newtype UnquotedString = Unquoted String
  deriving ( Eq, Ord )
  
instance Show UnquotedString where
  show (Unquoted s) = s
  
showUnquoted :: (Functor t, Show (t UnquotedString), 
  Show (t String)) => t String -> String
showUnquoted = fmap Unquoted >>> show

readonly :: forall m s a . Monad m => ReaderT s m a -> StateT s m a
readonly r = do
  s <- get
  lift $ runReaderT r s
  
stateless :: Monad m => StateT r m a -> ReaderT r m a
stateless st = do
  r <- ask
  lift (evalStateT st r)

stateful :: forall b r a m . Monad m => 
    StateT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
stateful st f = do
  r <- ask
  (a, r') <- lift $ runStateT st r
  local (const r') (f a)
  
statelocal :: State r b -> Reader r a -> Reader r a
statelocal st = local $ \r -> execState st r 

duplicates :: forall a . Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs) =
  case findAndRemove xs of
    Nothing -> duplicates xs
    Just xs' -> x : duplicates xs'
  where
    findAndRemove :: [a] -> Maybe [a]
    findAndRemove [] = Nothing
    findAndRemove (y:ys) 
      | x == y = Just (filter (/= x) ys)
      | otherwise = (y :) <$> findAndRemove ys

class IndentationMonad m where
  indent :: m a -> m a
  indentation :: IsString a => m a
  resetIndent :: m a -> m a
      
type Indented = Reader Int

instance IndentationMonad Indented where
  indent = local (+ 1)
  resetIndent = local (const 0)
  indentation = asks $ \i -> fromString $ "\n" ++ (concat . replicate i) "  "
  
runIndented :: Indented a -> a
runIndented = flip runReader 0

nubbed :: Eq a => [a] -> Bool
nubbed [] = True
nubbed (x:xs) =
  not (x `elem` xs) && nubbed xs

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = 
  let pxs = powerset xs
  in ((x :) <$> pxs) ++ pxs

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen = runST $ do
  g <- newSTRef gen
  let randomRST lohi = do
        (a,s') <- liftM (randomR lohi) (readSTRef g)
        writeSTRef g s'
        return a
  ar <- newArray n xs
  xs' <- forM [1..n] $ \i -> do
          j <- randomRST (i,n)
          vi <- readArray ar i
          vj <- readArray ar j
          writeArray ar j vi
          return vj
  gen' <- readSTRef g
  return (xs', gen')
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs
    
takeLast :: [a] -> ([a], a)
takeLast [x] = ([], x)
takeLast (x:xs) = first (x:) (takeLast xs)

class JSON a where
  json :: a -> String
  
xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True




