{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Tree.Modify
  ( insertBefore
  , insertAfter
  , delete
  ) where

import Data.Tree.AugmentedRBTree.Augment (Augment)
import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Tree.Search
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Modify
import Data.Tree.AugmentedRBTree.Zipper.Search
import Data.Tree.AugmentedRBTree.Zipper.Travel

lateInsertStage :: Augment v a => a -> Zipper v a -> Tree v a
lateInsertStage a z = tree where
  Zipper tree _ = mostUpZipper $ insertZipper a z

-- | Insert the given value into the tree.
--   If there're already equal values, insert the new one before them.
insertBefore :: (Ord a, Augment v a) => a -> Tree v a -> Tree v a
insertBefore a t =
  let
    z0 = searchTree a t
    z1 = partialDownLeftZipper $ shiftLeftMostEqZipper z0
    z2 = case z0 of
      Zipper Leave _ -> z0
      _ -> case z1 of
        Zipper Leave _ -> z1
        _ -> partialDownRightZipper $ mostDownRightBranchZipper z1
  in
    lateInsertStage a z2

-- | Insert the given value into the tree.
--   If there're already equal values, insert the new one after them.
insertAfter :: (Ord a, Augment v a) => a -> Tree v a -> Tree v a
insertAfter a t =
  let
    z0 = searchTree a t
    z1 = partialDownRightZipper $ shiftRightMostEqZipper z0
    z2 = case z0 of
      Zipper Leave _ -> z0
      _ -> case z1 of
        Zipper Leave _ -> z1
        _ -> partialDownLeftZipper $ mostDownLeftBranchZipper z1
  in
    lateInsertStage a z2

-- | Delete the given value in the tree. If the value is not present, the tree is untouched.
--   If found, it will delete the first found one (the nearest one to the root)
delete :: (Show a, Show v, Ord a, Augment v a) => a -> Tree v a -> Tree v a
delete a t = case searchTree a t of
  Zipper Leave _ -> t
  z -> let Zipper t' _ = mostUpZipper $ deleteZipper z in t'
