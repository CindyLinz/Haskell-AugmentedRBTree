{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Zipper.Modify where

import Data.Tree.AugmentedRBTree.Tree
import Data.Tree.AugmentedRBTree.Zipper
import Data.Tree.AugmentedRBTree.Zipper.Travel
import Data.Tree.AugmentedRBTree.Augment (Augment)

-- | Insert a new node to where the zipper point to, the zipper must be on a leave.
--   The returned zipper will point to the least common ancestor of all modified nodes.
insertZipper :: Augment v a => a -> Zipper v a -> Zipper v a
insertZipper a (Zipper _ ss) = insertFix (Zipper (branch red a leave leave) ss)
  where
    insertFix :: Augment v a => Zipper v a -> Zipper v a
    insertFix nZ =
      let
        Zipper n@(Branch nC nV nA nL nR) nS = nZ
        pZ@(Zipper p@(Branch pC pV pA pL pR) ~pS@(Step pD _ : _)) = partialUpZipper nZ
        gZ@(Zipper g@(Branch gC gV gA gL gR) gS) = partialUpZipper pZ
        u = if pD == dirLeft
          then gR
          else gL
        Branch uC uV uA uL uR = u
      in case nS of
        [] -> Zipper (Branch black nV nA nL nR) nS -- case 1
        Step nD _ : _ -> if pC == black
          then nZ -- case 2
          else if color u == red
            then -- case 3
              let
                gZ' = Zipper g' gS
                g' = Branch red gV gA gL' gR'
                  where
                    !(gL', gR') = if pD == dirLeft
                      then (p', u')
                      else (u', p')
                u' = Branch black uV uA uL uR
                p' = Branch black pV pA pL pR
              in
                insertFix gZ'
            else
              let
                case5 (Zipper n@(Branch _ nV nA nL nR) nS@(Step dirN (Branch _ pV pA pL pR) : _)) =
                  Zipper (branch black pA pL' pR') gS where
                    (pL', pR') = if dirN == dirLeft
                      then (n, branch red gA pR gR)
                      else (branch red gA gL pL, n)
              in
                if nD /= pD
                  then -- case 4
                    let
                      !(pL', pR') = if pD == dirLeft
                        then (pL, nL)
                        else (nR, pR)
                    in case5 (Zipper (branch red pA pL' pR') (Step pD n : pS))
                  else -- case 5
                    case5 nZ

-- | Delete a node which the zipper point to, the zipper must be on a branch.
--   The returned zipper will point to a starting rotation node.
deleteZipper :: Augment v a => Zipper v a -> Zipper v a
deleteZipper z@(Zipper (Branch c v a l r) ss) = case (l, r) of
  (Leave, r) -> if c == red then o else fixDelete o where o = Zipper r ss
  (l, Leave) -> if c == red then o else fixDelete o where o = Zipper l ss
  (_, _) -> deleteZipper victim
    where
      victim = mostDownLeftBranchZipper (partialDownRightZipper (Zipper (branch c victimA l r) ss))
      Zipper (Branch _ _ victimA _ _) _ = victim

  where
    fixDelete :: Augment v a => Zipper v a -> Zipper v a
    fixDelete = undefined
