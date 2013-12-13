module Data.Tree.AugmentedRBTree.Tree
  ( Tree (Leave, Branch)
  , Color (Red, Black)
  ) where

data Color = Red | Black
data Tree v a = Leave | Branch !Color !v !a (Tree v a) (Tree v a)
