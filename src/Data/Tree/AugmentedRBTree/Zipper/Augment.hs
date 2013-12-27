{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Zipper.Augment where

import Data.Tree.AugmentedRBTree.Augment
import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Travel

-- | The augment of the zipper node (must be on branch) and all its left.
augmentToLeft :: Augment v a => Zipper v a -> v
augmentToLeft z@(Zipper (Branch _ _ a l _) next) = go baseV z where
  baseV = build (augment l) a Nothing
  go v z@(Zipper _ s) = case s of
    StepFirst -> v
    Step dir _ _ -> go nextV nextZ where
      nextZ = partialUpZipper z
      Zipper (Branch _ _ a l _) _ = nextZ
      !nextV = if dir == dirLeft
        then v
        else build (augment l) a (Just v)

-- | The augment of the zipper node (must be on branch) and all its right.
augmentToRight :: Augment v a => Zipper v a -> v
augmentToRight z@(Zipper (Branch _ _ a _ r) next) = go baseV z where
  baseV = build Nothing a (augment r)
  go v z@(Zipper _ s) = case s of
    StepFirst -> v
    Step dir _ _ -> go nextV nextZ where
      nextZ = partialUpZipper z
      Zipper (Branch _ _ a _ r) _ = nextZ
      !nextV = if dir == dirLeft
        then build (Just v) a (augment r)
        else v

-- | The augment of all the nodes in the range (must not be an empty range)
--augmentRange :: Augment v a => ZipperRange v a -> v
