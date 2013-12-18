module Data.Tree.AugmentedRBTree.Tree where

import Data.Word (Word8)

import Data.Tree.AugmentedRBTree.Augment

newtype Color = Color Word8 deriving Eq
red = Color 1
black = Color 2

instance Show Color where
  show c = if c == red then "*" else "."

data Tree v a
  = Leave
    -- | v is the node augmented value (need to be non-strict to avoid unnecessary calculation when rotating the tree)
  | Branch {-# UNPACK #-} !Color v a (Tree v a) (Tree v a)
  deriving Show

-- | The value field of a node
value :: Tree v a -> Maybe a
value Leave = Nothing
value (Branch _ _ a _ _) = Just a

-- | The augment field of a node
augment :: Tree v a -> Maybe v
augment Leave = Nothing
augment (Branch _ v _ _ _) = Just v

-- | The color of a node
color :: Tree v a -> Color
color Leave = black
color (Branch c _ _ _ _) = c

-- | Smart constructor, identical to Leave
leave :: Tree v a
leave = Leave

-- | Smart constructor, create a Branch and corresponded augment field (non-strict)
branch :: Augment v a => Color -> a -> Tree v a -> Tree v a -> Tree v a
branch c a l r = Branch c (buildAugment l a r) a l r

-- | Build the augment from one node value and other two nodes (should be the two children in general)
buildAugment :: Augment v a => Tree v a -> a -> Tree v a -> v
buildAugment l a r = build (augment l) a (augment r)

-- | Create an empty tree
empty :: Tree v a
empty = leave

-- | Create a tree with only one value
singleton :: Augment v a => a -> Tree v a
singleton a = branch black a leave leave

-- | Create a tree from ordered list.
--   The precondition is not checked
--fromAscList :: Augment v a => [a] -> Tree v a
--fromAscList as = fromSubtree (map singleton as) where
