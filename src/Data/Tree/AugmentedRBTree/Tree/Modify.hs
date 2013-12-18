{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Tree.Modify
  ( insertBefore
  , insertAfter
  ) where

import Data.Tree.AugmentedRBTree.Augment (Augment)
import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Modify
import Data.Tree.AugmentedRBTree.Zipper.Search
import Data.Tree.AugmentedRBTree.Zipper.Travel

earlyInsertStage :: Ord a => a -> Tree v a -> Zipper v a
earlyInsertStage a t = searchZipper a $ initZipper t

lateInsertStage :: Augment v a => a -> Zipper v a -> Tree v a
lateInsertStage a z = tree where
  Zipper tree _ = mostUpZipper $ insertZipper a z

insertBefore :: (Ord a, Augment v a) => a -> Tree v a -> Tree v a
insertBefore a t =
  let
    z0 = earlyInsertStage a t
    z1 = partialDownLeftZipper $ shiftLeftMostEqZipper z0
    z2 = case z0 of
      Zipper Leave _ -> z0
      _ -> case z1 of
        Zipper Leave _ -> z1
        _ -> partialDownRightZipper $ mostDownRightBranchZipper z1
  in
    lateInsertStage a z2

insertAfter :: (Ord a, Augment v a) => a -> Tree v a -> Tree v a
insertAfter a t =
  let
    z0 = earlyInsertStage a t
    z1 = partialDownRightZipper $ shiftRightMostEqZipper z0
    z2 = case z0 of
      Zipper Leave _ -> z0
      _ -> case z1 of
        Zipper Leave _ -> z1
        _ -> partialDownLeftZipper $ mostDownLeftBranchZipper z1
  in
    lateInsertStage a z2
