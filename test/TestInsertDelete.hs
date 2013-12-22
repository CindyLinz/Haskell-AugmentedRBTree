module Main where

import System.Exit (exitFailure, exitSuccess)
import Data.List

import Test.QuickCheck

import Data.Tree.AugmentedRBTree.Augment
import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Tree.Modify as TM
import Data.Tree.AugmentedRBTree.Tree.Check

insertL :: (Ord a, Augment v a) => [a] -> [Tree v a]
insertL ns = scanl (\t n -> insertBefore n t) empty ns

insertR :: (Ord a, Augment v a) => [a] -> [Tree v a]
insertR ns = scanl (\t n -> insertAfter n t) empty ns

delete :: (Show a, Show v, Ord a, Augment v a) => [a] -> Tree v a -> [Tree v a]
delete ns t = scanl (\t n -> TM.delete n t) t ns

quickCheckInsert :: [Int] -> Bool
quickCheckInsert ns = all isValid treesl && all isValid treesr && all isValid treesd where
  treesl = insertL ns :: [Tree (Sum Int) Int]
  treesr = insertR ns :: [Tree (Sum Int) Int]
  treesd = Main.delete ns $ foldl (\t n -> insertBefore n t) empty ns :: [Tree (Sum Int) Int]

main = do
  res <- quickCheckResult quickCheckInsert
  case res of
    Failure {} -> exitFailure
    _ -> exitSuccess
