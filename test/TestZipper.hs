module Main where

import System.Exit (exitFailure, exitSuccess)

import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper

dumpTree :: Show a => Tree v a -> String
dumpTree t = go 0 t where
  go :: Show a => Int -> Tree v a -> String
  go ind Leave = take (ind * 2) (repeat ' ') ++ "#\n"
  go ind (Branch _ _ a l r) = go (ind + 1) l ++ take (ind * 2) (repeat ' ') ++ show a ++ "\n" ++ go (ind + 1) r

empty = Leave :: Tree Int Int
singleton = Branch Red 5 5 Leave Leave
leftLine = foldr (\n t -> Branch Red 1 n t Leave) Leave [1..9]
rightLine = foldr (\n t -> Branch Red 1 n Leave t) Leave [1..9]
normal =
  Branch Red 0 4
    (Branch Red 0 1
      Leave
      (Branch Red 0 3
        (Branch Red 0 2 Leave Leave)
        Leave
      )
    )
    (Branch Red 0 7
      (Branch Red 0 5
        Leave
        (Branch Red 0 6 Leave Leave)
      )
      Leave
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

  putStr "next - Nothing: "
  case nextBranchZipper (partialDownRightZipper (initZipper normal)) of
    Nothing -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next - Just right left: "
  case nextBranchZipper (partialDownLeftZipper (initZipper normal)) of
    Just (Zipper (Branch _ _ 2 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "next - Just right ancestor: "
  case nextBranchZipper (partialDownRightZipper (partialDownLeftZipper (initZipper normal))) of
    Just (Zipper (Branch _ _ 4 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev - Just left right: "
  case prevBranchZipper (partialDownRightZipper (initZipper normal)) of
    Just (Zipper (Branch _ _ 6 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  putStr "prev - Just left ancestor: "
  case prevBranchZipper (partialDownLeftZipper (partialDownRightZipper (initZipper normal))) of
    Just (Zipper (Branch _ _ 4 _ _) _) -> putStr "pass.\n"
    _ -> putStr "fail.\n" >> exitFailure

  exitSuccess
