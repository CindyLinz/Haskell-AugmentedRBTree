{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Tree.AugmentedRBTree.Augment where

class Augment v a where
  build :: (Maybe v) -> a -> (Maybe v) -> v
