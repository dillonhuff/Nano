module OptimizationGroups.AVXLevel1(avxLvl1Opts) where

import Data.List as L

import BlockDot
import Blocking
import CompactTemps
import DeleteRedundantAssignments
import Dummies
import Fusion
import IndexExpression
import InterchangeAndFuse
import LoopInvariantCodeMotion
import Matrix
import Operations
import RegisterizeTemps
import Registerization
import SMulToBroadcast
import SplitTemps
import Statement

avxLvl1Opts n =
  (registerization n) ++ (tempReductionAVX n) ++ (blockAndFuseAVXLvl1 n)

registerization n =
  [pullCodeOutOfLoops, deleteRedundantAssignments, registerizeBelow n "k_", registerize n "r_", smulToBroadcast "sm"]

tempReductionAVX n =
  [registerizeTempsBelow n, registerizeTemps n, compactTemps, splitTemps "t_"]

blockAndFuseAVXLvl1 n =
  interchangeAndFuse:(blockDot n "d_"):interchangeAndFuse:(L.intersperse interchangeAndFuse (blockingOptimizationsAVXLVL1 n))

blockingOptimizationsAVXLVL1 :: Int -> [[Statement] -> [Statement]]
blockingOptimizationsAVXLVL1 n =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddM, iConst n),
   (blockMatrixAddN, iConst n),
   (blockScalarMultiplyM, iConst n),
   (blockScalarMultiplyN, iConst n),
   (blockMatrixTransposeM, iConst n),
   (blockMatrixTransposeM, iConst n),
   (blockMatrixTransposeN, iConst n),
   (blockSetZeroM, iConst n),
   (blockSetZeroN, iConst n)]
