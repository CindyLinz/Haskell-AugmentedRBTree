module Data.Tree.AugmentedRBTree.Tree.Search where

import Data.Tree.AugmentedRBTree.Augment (Augment)
import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Search
import Data.Tree.AugmentedRBTree.Zipper.Travel

searchTree :: Ord a => a -> Tree v a -> Zipper v a
searchTree a t = searchZipper a $ initZipper t
