module Main where

import System.Exit (exitFailure, exitSuccess)
import Data.List

import Test.QuickCheck

import Data.Tree.AugmentedRBTree.Augment
import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Tree.Modify
import Data.Tree.AugmentedRBTree.Tree.Check

insertL :: (Ord a, Augment v a) => [a] -> [Tree v a]
insertL ns = scanl (\t n -> insertBefore n t) empty ns

insertR :: (Ord a, Augment v a) => [a] -> [Tree v a]
insertR ns = scanl (\t n -> insertAfter n t) empty ns

quickCheckInsert :: [Int] -> Bool
quickCheckInsert ns = all isValid treesl && all isValid treesr where
  treesl = insertL ns :: [Tree (Sum Int) Int]
  treesr = insertR ns :: [Tree (Sum Int) Int]

main = do
  res <- quickCheckResult quickCheckInsert
  case res of
    Failure {} -> exitFailure
    _ -> exitSuccess
