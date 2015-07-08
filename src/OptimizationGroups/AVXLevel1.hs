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

avxLvl1Opts =
  registerization ++ tempReductionAVX ++ blockAndFuseAVXLvl1

registerization =
  [pullCodeOutOfLoops, deleteRedundantAssignments, registerizeBelow 4 "k_", registerize 4 "r_", smulToBroadcast "sm"]

tempReductionAVX =
  [registerizeTempsBelow 4, registerizeTemps 4, compactTemps, splitTemps "t_"]

blockAndFuseAVXLvl1 =
  interchangeAndFuse:(blockDot 4 "d_"):interchangeAndFuse:(L.intersperse interchangeAndFuse blockingOptimizationsAVXLVL1)

blockingOptimizationsAVXLVL1 :: [[Statement] -> [Statement]]
blockingOptimizationsAVXLVL1 =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddM, iConst 4),
   (blockMatrixAddN, iConst 4),
   (blockScalarMultiplyM, iConst 4),
   (blockScalarMultiplyN, iConst 4),
   (blockMatrixTransposeM, iConst 4),
   (blockMatrixTransposeM, iConst 4),
   (blockMatrixTransposeN, iConst 4),
   (blockSetZeroM, iConst 4),
   (blockSetZeroN, iConst 4)]
