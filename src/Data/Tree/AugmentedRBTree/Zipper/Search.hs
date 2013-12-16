{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Zipper.Search where

import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Travel

-- | If found, return the first (nearest to the root) found branch node
--   If not found, return the ready for insert leave.
--
--   You can go furthur get the left most or right most branch node with equal value
--   by 'shiftLeftMostEqZipper' and 'shiftRightMostEqZipper'
--
--   Note, it will only search the subtree that the passed zipper pointed to.
--   So, you should use it from the root.
searchZipper :: Ord a => a -> Zipper v a -> Zipper v a
searchZipper _ z@(Zipper Leave _) = z
searchZipper target z@(Zipper (Branch _ _ a l r) _) = case compare target a of
  LT -> searchZipper target (partialDownLeftZipper z)
  GT -> searchZipper target (partialDownRightZipper z)
  EQ -> z

-- | Shift the branch zipper to the left most branch zipper that has equal value
--
--   Note that it will only search the subtree that the passed zipper pointed to.
--   So, you should pass the nearest to the root branch zipper
shiftLeftMostEqZipper :: Eq a => Zipper v a -> Zipper v a
shiftLeftMostEqZipper z@(Zipper (Branch _ _ target _ _) _) = go z next where
  !next = partialDownLeftZipper z
  go last curr@(Zipper currTree _) = case currTree of
    Leave -> last
    Branch _ _ a _ _ -> if a == target
      then let !next = partialDownLeftZipper curr in go curr next
      else let !next = partialDownRightZipper curr in go last next

-- | Shift the branch zipper to the right most branch zipper that has equal value
--
--   Note that it will only search the subtree that the passed zipper pointed to.
--   So, you should pass the nearest to the root branch zipper
shiftRightMostEqZipper :: Eq a => Zipper v a -> Zipper v a
shiftRightMostEqZipper z@(Zipper (Branch _ _ target _ _) _) = go z next where
  !next = partialDownRightZipper z
  go last curr@(Zipper currTree _) = case currTree of
    Leave -> last
    Branch _ _ a _ _ -> if a == target
      then let !next = partialDownRightZipper curr in go curr next
      else let !next = partialDownLeftZipper curr in go last next
