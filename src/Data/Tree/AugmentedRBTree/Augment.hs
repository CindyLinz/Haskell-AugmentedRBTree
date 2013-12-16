{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls #-}
module Data.Tree.AugmentedRBTree.Augment where

-- | Augment v a means that type v could be augmented values from type a
class Augment v a where
  build :: (Maybe v) -> a -> (Maybe v) -> v

-- * Sample Augment
{- $
   Below are some sample augments, you can use them directly or define your own
 -}

-- | No constructors, if you are sure that you won't touch the augmented value
--   then you can use it
data Undefined
instance Eq Undefined where
  (==) = undefined
instance Show Undefined where
  show = undefined
instance Augment Undefined a where
  build _ _ _ = undefined

-- | Always Identity, a boring augmented value
data Identity = Identity deriving (Show, Eq)
instance Augment Identity a where
  build _ _ _ = Identity

-- | The maximum of the subtree
newtype Max a = Max a deriving (Show, Eq)
instance Augment (Max a) a where
  build _ _ (Just (Max r)) = Max r
  build _ a Nothing = Max a

-- | The minimum of the subtree
newtype Min a = Min a deriving (Show, Eq)
instance Augment (Min a) a where
  build (Just (Min l)) _ _ = Min l
  build Nothing a _ = Min a

-- | The size (number of branch nodes) of the subtree
newtype Count a = Count a deriving (Show, Eq)
instance Num a => Augment (Count a) b where
  build Nothing a Nothing = Count 1
  build (Just (Count l)) a Nothing = Count (l + 1)
  build Nothing a (Just (Count r)) = Count (1 + r)
  build (Just (Count l)) a (Just (Count r)) = Count (l + 1 + r)

-- | The total sum of the values of the subtree
newtype Sum a = Sum a deriving (Show, Eq)
instance Num a => Augment (Sum a) a where
  build Nothing a Nothing = Sum a
  build (Just (Sum l)) a Nothing = Sum (l + a)
  build Nothing a (Just (Sum r)) = Sum (a + r)
  build (Just (Sum l)) a (Just (Sum r)) = Sum (l + a + r)
