module Data.Tree.AugmentedRBTree.Zipper.Travel where

import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Augment (Augment)

-- | Move the zipper up, without checking
partialUpZipper :: Augment v a => Zipper v a -> Zipper v a
partialUpZipper (Zipper curr (Step d (Branch c v a l r) : ss)) = Zipper (branch c a l' r') ss
  where
    (l', r') = if d == dirLeft
      then (curr, r)
      else (l, curr)

-- | Move the zipper up, without checking, readonly vesion.
partialUpZipper' :: Zipper v a -> Zipper v a
partialUpZipper' (Zipper _ (Step _ t : ss)) = Zipper t ss

-- | Move the zipper, with checking
upZipper :: Augment v a => Zipper v a -> Maybe (Zipper v a)
upZipper (Zipper _ []) = Nothing
upZipper z = Just (partialUpZipper z)

-- | Move the zipper, with checking, readonly version.
upZipper' :: Zipper v a -> Maybe (Zipper v a)
upZipper' (Zipper _ []) = Nothing
upZipper' z = Just (partialUpZipper' z)

-- | Move the zipper to the left branch, without checking
partialDownLeftZipper :: Zipper v a -> Zipper v a
partialDownLeftZipper (Zipper t@(Branch _ _ _ next _) ss) = Zipper next (Step dirLeft t : ss)

-- | Move the zipper to the left branch, with checking
downLeftZipper :: Zipper v a -> Maybe (Zipper v a)
downLeftZipper (Zipper Leave _) = Nothing
downLeftZipper z = Just (partialDownLeftZipper z)

-- | Move the zipper to the right branch, without checking
partialDownRightZipper :: Zipper v a -> Zipper v a
partialDownRightZipper (Zipper t@(Branch _ _ _ _ next) ss) = Zipper next (Step dirRight t : ss)

-- | Move the zipper to the right branch, with checking
downRightZipper :: Zipper v a -> Maybe (Zipper v a)
downRightZipper (Zipper Leave _) = Nothing
downRightZipper z = Just (partialDownRightZipper z)

-- | Move the zipper up to the least ancestor right before it step left
backLastLeftZipper :: Augment v a => Zipper v a -> Maybe (Zipper v a)
backLastLeftZipper (Zipper _ []) = Nothing
backLastLeftZipper z@(Zipper _ (Step dir _ : _)) =
  if dir == dirLeft
    then Just upper
    else backLastLeftZipper upper
  where upper = partialUpZipper z

-- | Move the zipper up to the least ancestor right before it step left, readonly version.
backLastLeftZipper' :: Zipper v a -> Maybe (Zipper v a)
backLastLeftZipper' (Zipper _ []) = Nothing
backLastLeftZipper' z@(Zipper _ (Step dir _ : _)) =
  if dir == dirLeft
    then Just upper
    else backLastLeftZipper' upper
  where upper = partialUpZipper' z

-- | Move the zipper to the least ancestor right before it step right
backLastRightZipper :: Augment v a => Zipper v a -> Maybe (Zipper v a)
backLastRightZipper (Zipper _ []) = Nothing
backLastRightZipper z@(Zipper _ (Step dir _ : _)) =
  if dir == dirRight
    then Just upper
    else backLastRightZipper upper
  where upper = partialUpZipper z

-- | Move the zipper to the least ancestor right before it step right, readonly version.
backLastRightZipper' :: Zipper v a -> Maybe (Zipper v a)
backLastRightZipper' (Zipper _ []) = Nothing
backLastRightZipper' z@(Zipper _ (Step dir _ : _)) =
  if dir == dirRight
    then Just upper
    else backLastRightZipper' upper
  where upper = partialUpZipper' z

-- | Move the zipper back to the root
mostUpZipper :: Augment v a => Zipper v a -> Zipper v a
mostUpZipper z@(Zipper _ []) = z
mostUpZipper z = mostUpZipper $ partialUpZipper z

-- | Move the zipper from a branch node to the most down-left branch node.
--   The caller is responsible to make sure the original zipper is on a branch node.
mostDownLeftBranchZipper :: Zipper v a -> Zipper v a
mostDownLeftBranchZipper z@(Zipper (Branch _ _ _ Leave _) _) = z
mostDownLeftBranchZipper (Zipper t@(Branch _ _ _ t1 _) ss) = mostDownLeftBranchZipper (Zipper t1 (Step dirLeft t : ss))

-- | Move the zipper from a branch node to the most down-right branch node.
--   The caller is responsible to make sure the original zipper is on a branch.
mostDownRightBranchZipper :: Zipper v a -> Zipper v a
mostDownRightBranchZipper z@(Zipper (Branch _ _ _ _ Leave) _) = z
mostDownRightBranchZipper (Zipper t@(Branch _ _ _ _ t1) ss) = mostDownRightBranchZipper (Zipper t1 (Step dirRight t : ss))

-- | Move a zipper from a branch node to the next branch node.
--   The caller is responsible to make sure the original zipper is on a branch node.
nextBranchZipper :: Augment v a => Zipper v a -> Maybe (Zipper v a)
nextBranchZipper z@(Zipper (Branch _ _ _ _ Leave) _) = backLastLeftZipper z
nextBranchZipper z = nextBranchZipper' z

-- | Move a zipper from a branch node to the next branch node. readonly version
--   The caller is responsible to make sure the original zipper is on a branch node.
nextBranchZipper' :: Zipper v a -> Maybe (Zipper v a)
nextBranchZipper' z@(Zipper (Branch _ _ _ _ Leave) _) = backLastLeftZipper' z
nextBranchZipper' z = Just $ mostDownLeftBranchZipper (partialDownRightZipper z)

-- | Move a zipper from a branch node to the previous branch node.
--   The caller is responsible to make sure the original zipper is on a branch node.
prevBranchZipper :: Augment v a => Zipper v a -> Maybe (Zipper v a)
prevBranchZipper z@(Zipper (Branch _ _ _ Leave _) _) = backLastRightZipper z
prevBranchZipper z = prevBranchZipper' z

-- | Move a zipper from a branch node to the previous branch node. readonly version
--   The caller is responsible to make sure the original zipper is on a branch node.
prevBranchZipper' :: Zipper v a -> Maybe (Zipper v a)
prevBranchZipper' z@(Zipper (Branch _ _ _ Leave _) _) = backLastRightZipper' z
prevBranchZipper' z = Just $ mostDownRightBranchZipper (partialDownLeftZipper z)
