module Data.Tree.AugmentedRBTree.Zipper
  ( Zipper (Zipper)
  , initZipper
  , partialUpZipper
  , upZipper
  , partialDownLeftZipper
  , downLeftZipper
  , partialDownRightZipper
  , downRightZipper
  , backLastLeftZipper
  , backLastRightZipper
  , mostDownLeftBranchZipper
  , mostDownRightBranchZipper
  , nextBranchZipper
  , prevBranchZipper
  ) where

import Data.Tree.AugmentedRBTree.Tree

data Dir = GoLeft | GoRight
data Step v a = Step !Dir (Tree v a)
data Zipper v a = Zipper (Tree v a) ![Step v a]

-- | Get a zipper on the root node
initZipper :: Tree v a -> Zipper v a
initZipper t = Zipper t []

-- | Move the zipper up, without checking
partialUpZipper :: Zipper v a -> Zipper v a
partialUpZipper (Zipper _ (Step _ t : ss)) = Zipper t ss

-- | Move the zipper, with checking
upZipper :: Zipper v a -> Maybe (Zipper v a)
upZipper (Zipper _ []) = Nothing
upZipper z = Just (partialUpZipper z)

-- | Move the zipper to the left branch, without checking
partialDownLeftZipper :: Zipper v a -> Zipper v a
partialDownLeftZipper (Zipper t@(Branch _ _ _ next _) ss) = Zipper next (Step GoLeft t : ss)

-- | Move the zipper to the left branch, with checking
downLeftZipper :: Zipper v a -> Maybe (Zipper v a)
downLeftZipper (Zipper Leave _) = Nothing
downLeftZipper z = Just (partialDownLeftZipper z)

-- | Move the zipper to the right branch, without checking
partialDownRightZipper :: Zipper v a -> Zipper v a
partialDownRightZipper (Zipper t@(Branch _ _ _ _ next) ss) = Zipper next (Step GoRight t : ss)

-- | Move the zipper to the right branch, with checking
downRightZipper :: Zipper v a -> Maybe (Zipper v a)
downRightZipper (Zipper Leave _) = Nothing
downRightZipper z = Just (partialDownRightZipper z)

-- | Move the zipper to the least ancestor right before it step left
backLastLeftZipper :: Zipper v a -> Maybe (Zipper v a)
backLastLeftZipper (Zipper _ []) = Nothing
backLastLeftZipper (Zipper _ (Step GoLeft t : ss)) = Just (Zipper t ss)
backLastLeftZipper (Zipper _ (Step GoRight t : ss)) = backLastLeftZipper (Zipper t ss)

-- | Move the zipper to the least ancestor right before it step right
backLastRightZipper :: Zipper v a -> Maybe (Zipper v a)
backLastRightZipper (Zipper _ []) = Nothing
backLastRightZipper (Zipper _ (Step GoRight t : ss)) = Just (Zipper t ss)
backLastRightZipper (Zipper _ (Step GoLeft t : ss)) = backLastRightZipper (Zipper t ss)

-- | Move the zipper from a branch node to the most down-left branch node.
--   The caller is responsible to make sure the original zipper is on a branch node.
mostDownLeftBranchZipper :: Zipper v a -> Zipper v a
mostDownLeftBranchZipper z@(Zipper (Branch _ _ _ Leave _) _) = z
mostDownLeftBranchZipper (Zipper t@(Branch _ _ _ t1 _) ss) = Zipper t1 (Step GoLeft t : ss)

-- | Move the zipper from a branch node to the most down-right branch node.
--   The caller is responsible to make sure the original zipper is on a branch.
mostDownRightBranchZipper :: Zipper v a -> Zipper v a
mostDownRightBranchZipper z@(Zipper (Branch _ _ _ _ Leave) _) = z
mostDownRightBranchZipper (Zipper t@(Branch _ _ _ _ t1) ss) = Zipper t1 (Step GoRight t : ss)

-- | Move a zipper from a branch node to the next branch node.
--   The caller is responsible to make sure the original zipper is on a branch node.
nextBranchZipper :: Zipper v a -> Maybe (Zipper v a)
nextBranchZipper z@(Zipper (Branch _ _ _ _ Leave) _) = backLastLeftZipper z
nextBranchZipper z = Just $ mostDownLeftBranchZipper (partialDownRightZipper z)

-- | Move a zipper from a branch node to the previous branch node.
--   The caller is responsible to make sure the original zipper is on a branch node.
prevBranchZipper :: Zipper v a -> Maybe (Zipper v a)
prevBranchZipper z@(Zipper (Branch _ _ _ Leave _) _) = backLastRightZipper z
prevBranchZipper z = Just $ mostDownRightBranchZipper (partialDownLeftZipper z)
