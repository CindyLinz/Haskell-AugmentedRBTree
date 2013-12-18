{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Main where

import System.Exit (exitFailure, exitSuccess)

import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Travel
import Data.Tree.AugmentedRBTree.Augment

instance Augment Int Int where
  build Nothing !a Nothing = a
  build Nothing a (Just r) = let !o = a + r in o
  build (Just l) a Nothing = let !o = l + a in o
  build (Just l) a (Just r) = let !o = l + a + r in o

dumpTree :: Show a => Tree v a -> String
dumpTree t = go 0 t where
  go :: Show a => Int -> Tree v a -> String
  go ind Leave = take (ind * 2) (repeat ' ') ++ "#\n"
  go ind (Branch _ _ a l r) = go (ind + 1) l ++ take (ind * 2) (repeat ' ') ++ show a ++ "\n" ++ go (ind + 1) r

empty, singleton, leftLine, rightLine, normal :: Tree Int Int
empty = leave
singleton = branch red 5 leave leave
leftLine = foldr (\n t -> branch red n t leave) leave [1..9]
rightLine = foldr (\n t -> branch red n leave t) leave [1..9]
normal =
  branch red 4
    (branch red 1
      leave
      (branch red 3
        (branch red 2 leave leave)
        leave
      )
    )
    (branch red 7
      (branch red 5
        leave
        (branch red 6 leave leave)
      )
      leave
    )

main = do
  putStr $ "empty:\n" ++ dumpTree empty
  putStr $ "singleton:\n" ++ dumpTree singleton
  putStr $ "leftLine:\n" ++ dumpTree leftLine
  putStr $ "rightLine:\n" ++ dumpTree rightLine
  putStr $ "normal:\n" ++ dumpTree normal

  putStr "upZipper (partialUpZipper) - Nothing: "
  case upZipper (initZipper empty) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "upZipper' (partialUpZipper') - Nothing: "
  case upZipper' (initZipper empty) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "downLeftZipper (partialDownLeftZipper) - Nothing: "
  case downLeftZipper (initZipper empty) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "downRightZipper (partialDownRightZipper) - Nothing: "
  case downRightZipper (initZipper empty) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev - Nothing: "
  case prevBranchZipper (partialDownLeftZipper (initZipper normal)) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev' - Nothing: "
  case prevBranchZipper' (partialDownLeftZipper (initZipper normal)) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next - Nothing: "
  case nextBranchZipper (partialDownRightZipper (initZipper normal)) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next' - Nothing: "
  case nextBranchZipper' (partialDownRightZipper (initZipper normal)) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next - Just right left: "
  case nextBranchZipper (partialDownLeftZipper (initZipper normal)) of
    Just (Zipper (Branch _ _ 2 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next' - Just right left: "
  case nextBranchZipper' (partialDownLeftZipper (initZipper normal)) of
    Just (Zipper (Branch _ _ 2 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next - Just right ancestor: "
  case nextBranchZipper (partialDownRightZipper (partialDownLeftZipper (initZipper normal))) of
    Just (Zipper (Branch _ _ 4 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next' - Just right ancestor: "
  case nextBranchZipper' (partialDownRightZipper (partialDownLeftZipper (initZipper normal))) of
    Just (Zipper (Branch _ _ 4 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev - Just left right: "
  case prevBranchZipper (partialDownRightZipper (initZipper normal)) of
    Just (Zipper (Branch _ _ 6 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev' - Just left right: "
  case prevBranchZipper' (partialDownRightZipper (initZipper normal)) of
    Just (Zipper (Branch _ _ 6 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev - Just left ancestor: "
  case prevBranchZipper (partialDownLeftZipper (partialDownRightZipper (initZipper normal))) of
    Just (Zipper (Branch _ _ 4 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev' - Just left ancestor: "
  case prevBranchZipper' (partialDownLeftZipper (partialDownRightZipper (initZipper normal))) of
    Just (Zipper (Branch _ _ 4 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  exitSuccess
