module Data.Tree.AugmentedRBTree.Cursor where

import Data.Tree.AugmentedRBTree.Zipper (Zipper)
import Data.Tree.AugmentedRBTree.Zipper.Travel

newtype Cursor v a = Cursor (Zipper v a)
