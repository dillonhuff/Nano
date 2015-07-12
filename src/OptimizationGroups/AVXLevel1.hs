module OptimizationGroups.AVXLevel1(avxLvl1Opts) where

import Data.List as L

import Transformations.BlockDot
import Transformations.Blocking
import Transformations.CompactTemps
import Transformations.DeleteRedundantAssignments
import Dummies
import Transformations.Fusion
import IndexExpression
import Transformations.InterchangeAndFuse
import Transformations.LoopInvariantCodeMotion
import Matrix
import Operations
import Transformations.RegisterizeTemps
import Transformations.Registerization
import Transformations.SMulToBroadcast
import Transformations.SplitTemps
import Statement
import Transformations.StatementInterchange

avxLvl1Opts n =
  (registerization n) ++ (tempReductionAVX n) ++ (blockAndFuseAVXLvl1 n)

registerization n =
  [pullCodeOutOfLoops, deleteRedundantAssignments, registerizeBelow n "k_", registerize n "r_", smulToBroadcast "sm"]
--  [deleteRedundantAssignments, registerizeBelow n "k_", registerize n "r_", smulToBroadcast "sm"]  

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
   (blockMatrixTransposeN, iConst n),
   (blockSetZeroM, iConst n),
   (blockSetZeroN, iConst n),
   (blockSetM, iConst n),
   (blockSetN, iConst n)]
