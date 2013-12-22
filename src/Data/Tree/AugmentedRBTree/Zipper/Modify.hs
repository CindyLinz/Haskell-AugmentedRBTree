{-# LANGUAGE BangPatterns #-}
module Data.Tree.AugmentedRBTree.Zipper.Modify where

import Data.Tree.AugmentedRBTree.Tree.Check

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
deleteZipper :: (Augment v a) => Zipper v a -> Zipper v a
deleteZipper z@(Zipper (Branch c v a l r) ss) = case (l, r) of
  (Leave, Leave) -> if c == red then Zipper Leave ss else deleteFix (Zipper Leave ss)
  (Leave, Branch _ v a _ _) -> Zipper (Branch black v a Leave Leave) ss
  (Branch _ v a _ _, Leave) -> Zipper (Branch black v a Leave Leave) ss
  (_, _) -> deleteZipper victim
    where
      victim = mostDownLeftBranchZipper (partialDownRightZipper (Zipper (branch c victimA l r) ss))
      Zipper (Branch _ _ victimA _ _) _ = victim

  where
    deleteFix :: (Augment v a) => Zipper v a -> Zipper v a
    deleteFix nZ =
      let
        Zipper n nS = nZ
        Branch nC nV nA nL nR = n
        pZ@(Zipper ~p@(Branch pC pV pA pL pR) pS) = partialUpZipper nZ
        Step nD _ : _ = nS
        s = if nD == dirLeft then pR else pL
        sC = color s
        Branch _ sV sA sL sR = s
      in case nS of
        [] -> nZ -- case 1
        _ -> if sC == red
          then -- case 2
            let
              (pL', pR', sL', sR') = if nD == dirLeft
                then (undefined, sL, undefined, sR)
                else (sR, undefined, sL, undefined)
            in tryCase3 $ Zipper n (Step nD (branch red pA pL' pR') : Step nD (branch black sA sL' sR') : pS)
          else
            tryCase3 nZ
          where
            tryCase3 nZ =
              let
                Zipper n nS = nZ
                Branch nC nV nA nL nR = n
                pZ@(Zipper p@(Branch pC pV pA pL pR) pS) = partialUpZipper nZ
                s@(Branch sC sV sA sL sR) = if nD == dirLeft then pR else pL
              in if color sL == black && color sR == black
                then
                  let
                    (pL', pR') = if nD == dirLeft
                      then (n, s')
                      else (s', n)
                    s' = Branch red sV sA sL sR
                  in if pC == black && color sL == black && color sR == black
                    then -- case 3
                      deleteFix $ Zipper (Branch pC pV pA pL' pR') pS
                    else -- case 4
                      Zipper (Branch black pV pA pL' pR') pS
                else
                  let
                    (pL', pR', moveDown) = if nD == dirLeft
                      then (n, branch black sLA sLL (branch red sA sLR sR), partialDownLeftZipper)
                      else (branch black sRA (branch red sA sL sRL) sRR, n, partialDownRightZipper)
                    Branch sLC sLV sLA sLL sLR = sL
                    Branch sRC sRV sRA sRL sRR = sR
                  in if nD == dirLeft && color sR == black || nD == dirRight && color sL == black
                    then -- case 5
                      doCase6 $ moveDown $ Zipper (branch pC pA pL' pR') pS
                    else
                      doCase6 nZ
              where
                doCase6 nZ = Zipper (branch pC sA sL' sR') pS -- case 6
                  where
                    Zipper n nS = nZ
                    Branch nC nV nA nL nR = n
                    pZ@(Zipper p@(Branch pC pV pA pL pR) pS) = partialUpZipper nZ
                    s@(Branch sC sV sA sL sR) = if nD == dirLeft then pR else pL
                    Branch sLC sLV sLA sLL sLR = sL
                    Branch sRC sRV sRA sRL sRR = sR
                    (sL', sR') = if nD == dirLeft
                      then (branch black pA n sL, Branch black sRV sRA sRL sRR)
                      else (Branch black sLV sLA sLL sLR, branch black pA sR n)

