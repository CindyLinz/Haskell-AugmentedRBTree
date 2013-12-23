module Data.Tree.AugmentedRBTree.Zipper where

import Data.Word (Word8)
import Data.Tree.AugmentedRBTree.Tree

newtype Dir = Dir Word8 deriving Eq
dirLeft = Dir 1
dirRight = Dir 2

instance Show Dir where
  show d = if d == dirLeft then "<-" else "->"

data Step v a = StepFirst | Step {-# UNPACK #-} !Dir (Tree v a) (Step v a) deriving Show
data Zipper v a = Zipper (Tree v a) (Step v a) deriving Show

-- | Get a zipper on the root node
initZipper :: Tree v a -> Zipper v a
initZipper t = Zipper t StepFirst

-- | Get the zipper node
zipperTree :: Zipper v a -> Tree v a
zipperTree (Zipper t _) = t

-- | Get the last direction, without checking
partialZipperDir :: Zipper v a -> Dir
partialZipperDir (Zipper _ (Step d _ _)) = d

-- | Get the last direction, with checking
zipperDir :: Zipper v a -> Maybe Dir
zipperDir (Zipper _ StepFirst) = Nothing
zipperDir z = Just $ partialZipperDir z
