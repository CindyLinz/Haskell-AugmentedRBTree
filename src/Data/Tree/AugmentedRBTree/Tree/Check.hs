{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Tree.Check where

import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Augment
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Travel
import Data.Tree.AugmentedRBTree.Zipper.Augment

isRootBlack :: Tree v a -> Bool
isRootBlack t = color t == black

-- | Count the number of black nodes from root to leave.
--   return Nothing if there's any path with different count.
balancedBlackCount :: Tree v a -> Maybe Int
balancedBlackCount Leave = Just 1
balancedBlackCount (Branch c _ _ l r) =
  case balancedBlackCount l of
    Nothing -> Nothing
    Just lCount -> case balancedBlackCount r of
      Nothing -> Nothing
      Just rCount -> if lCount /= rCount
        then Nothing
        else if c == black
          then Just $! lCount + 1
          else Just lCount

isBalanced :: Tree v a -> Bool
isBalanced t = case balancedBlackCount t of
  Just _ -> True
  Nothing -> False

-- | Check if there are no nearby red nodes.
isRedValid :: Tree v a -> Bool
isRedValid Leave = True
isRedValid (Branch c _ _ l r) =
  isRedValid l && isRedValid r && 
  ( c == black || color l == black && color r == black )

valueRange :: Ord a => Tree v a -> Maybe (Maybe (a, a))
valueRange Leave = Just Nothing
valueRange (Branch _ _ a l r) = result where
  (lGood, min) = case valueRange l of
    Nothing -> (False, undefined)
    Just lRange -> case lRange of
      Nothing -> (True, a)
      Just (lMin, lMax) -> (lMax <= a, lMin)
  (rGood, max) = case valueRange r of
    Nothing -> (False, undefined)
    Just rRange -> case rRange of
      Nothing -> (True, a)
      Just (rMin, rMax) -> (a <= rMin, rMax)
  result = if lGood && rGood
    then Just (Just (min, max))
    else Nothing

-- | Check node value order
isOrdered :: Ord a => Tree v a -> Bool
isOrdered t = case valueRange t of
  Nothing -> False
  Just _ -> True

isAugmentValid :: (Eq v, Augment v a) => Tree v a -> Bool
isAugmentValid Leave = True
isAugmentValid (Branch _ v a l r) =
  isAugmentValid l && isAugmentValid r &&
  buildAugment l a r == v

isAugmentLeftRangeValid :: (Eq v, Augment v a) => Tree v a -> Bool
isAugmentLeftRangeValid Leave = True
isAugmentLeftRangeValid t = go v start where
  start = (mostDownLeftBranchZipper (initZipper t))
  Zipper (Branch _ _ startA _ _) _ = start
  v = build Nothing startA Nothing
  go :: (Eq v, Augment v a) => v -> Zipper v a -> Bool
  go v z = testCurrent && testRemain where
    testCurrent = v == augmentToLeft z
    testRemain = case nextBranchZipper z of
      Nothing -> True
      Just nextZ -> go nextV nextZ where
        !nextV = build (Just v) nextA Nothing
        Zipper (Branch _ _ nextA _ _) _ = nextZ

isAugmentRightRangeValid :: (Show v, Eq v, Augment v a) => Tree v a -> Bool
isAugmentRightRangeValid Leave = True
isAugmentRightRangeValid t = go v start where
  start = (mostDownRightBranchZipper (initZipper t))
  Zipper (Branch _ _ startA _ _) _ = start
  v = build Nothing startA Nothing
  go :: (Show v, Eq v, Augment v a) => v -> Zipper v a -> Bool
  go v z = testCurrent && testRemain where
    testCurrent = if v == augmentToRight z
      then True
      else False
    testRemain = case prevBranchZipper z of
      Nothing -> True
      Just nextZ -> go nextV nextZ where
        !nextV = build Nothing nextA (Just v)
        Zipper (Branch _ _ nextA _ _) _ = nextZ

isValid :: (Show v, Show a, Eq v, Ord a, Augment v a) => Tree v a -> Bool
isValid t =
  isOrdered t &&
  isAugmentValid t &&
  isBalanced t &&
  isRootBlack t &&
  isRedValid t &&
  isAugmentLeftRangeValid t &&
  isAugmentRightRangeValid t
