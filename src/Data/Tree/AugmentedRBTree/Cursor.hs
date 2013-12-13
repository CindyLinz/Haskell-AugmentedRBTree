module Data.Tree.AugmentedRBTree.Cursor
  ( Cursor
  ) where

import Data.Tree.AugmentedRBTree.Zipper (Zipper)

newtype Cursor v a = Cursor (Zipper v a)
