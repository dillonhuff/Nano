module OptimizationGroups.Level1(lvl1Opts) where

import Data.List as L

import Dummies
import Transformations.Fusion
import Core.IndexExpression
import Core.Matrix
import Operations
import Core.Statement
import Transformations.BlockDot
import Transformations.Blocking
import Transformations.CompactTemps
import Transformations.DeleteRedundantAssignments
import Transformations.InterchangeAndFuse
import Transformations.LoopInvariantCodeMotion
import Transformations.RegisterizeTemps
import Transformations.IntroducePacking
import Transformations.SMulToBroadcast
import Transformations.SplitTemps
import Transformations.StatementInterchange

lvl1Opts n =
  (registerization n) ++ (tempReduction n) ++ (blockAndFuseLvl1 n)

registerization n =
  [pullCodeOutOfLoops, deleteRedundantAssignments, registerizeTemps n, pack n "r_"]

tempReduction n =
  [compactTemps, splitTemps "t_", smulToBroadcast "sm"]

blockAndFuseLvl1 n =
  interchangeAndFuse:(blockDot n "d_"):interchangeAndFuse:(L.intersperse interchangeAndFuse (blockingOptimizationsLVL1 n))

blockingOptimizationsLVL1 :: Int -> [[Statement] -> [Statement]]
blockingOptimizationsLVL1 n =
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
