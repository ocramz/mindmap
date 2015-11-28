module MindMap.BiMap where

import qualified Data.Map.Strict as M


data Bimap a b = MkBimap !(M.Map a b) !(M.Map b a)

instance (Show a, Show b) => Show (Bimap a b) where
  show (MkBimap x y) = show $ "BiMap " ++ show (x, y)

singleton :: a -> b -> Bimap a b
singleton k v = MkBimap (M.singleton k v) (M.singleton v k)

insert :: (Ord a, Ord b) => a -> b -> Bimap a b -> Bimap a b
insert k v (MkBimap l r) = MkBimap (M.insert k v l) (M.insert v k r) 

lookupK :: (Ord a) => a -> Bimap a b -> Maybe b
lookupK k (MkBimap l _) = M.lookup k l

lookupV :: (Ord b) => b -> Bimap a b -> Maybe a
lookupV v (MkBimap _ r) = M.lookup v r


size :: Bimap a b -> Int
size (MkBimap x _) = M.size x
