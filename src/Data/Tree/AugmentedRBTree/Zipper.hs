module Data.Tree.AugmentedRBTree.Zipper
  ( Zipper (Zipper)
  ) where

import Data.Tree.AugmentedRBTree.Tree

data Dir = Left | Right
data Step v a = Step !Dir (Tree v a)
data Zipper v a = Zipper (Tree v a) ![Step v a]
