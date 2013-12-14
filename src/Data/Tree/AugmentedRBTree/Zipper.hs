module Data.Tree.AugmentedRBTree.Zipper where

import Data.Word (Word8)
import Data.Tree.AugmentedRBTree.Tree

newtype Dir = Dir Word8 deriving Eq
dirLeft = Dir 1
dirRight = Dir 2

instance Show Dir where
  show d = if d == dirLeft then "<-" else "->"

data Step v a = Step {-# UNPACK #-} !Dir (Tree v a) deriving Show
data Zipper v a = Zipper (Tree v a) ![Step v a] deriving Show

-- | Get a zipper on the root node
initZipper :: Tree v a -> Zipper v a
initZipper t = Zipper t []
