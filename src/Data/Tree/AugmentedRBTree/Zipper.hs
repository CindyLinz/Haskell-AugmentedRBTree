module Data.Tree.AugmentedRBTree.Zipper
  ( Zipper (Zipper)
  , Dir (GoLeft, GoRight)
  , Step (Step)
  , initZipper
  ) where

import Data.Tree.AugmentedRBTree.Tree

data Dir = GoLeft | GoRight
data Step v a = Step !Dir (Tree v a)
data Zipper v a = Zipper (Tree v a) ![Step v a]

-- | Get a zipper on the root node
initZipper :: Tree v a -> Zipper v a
initZipper t = Zipper t []
