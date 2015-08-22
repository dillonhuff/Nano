module SystemTests.MicroBlockedAVX(allMicroBlockedAVXTests) where

import Data.List as L
import Test.HUnit

import CBackEnd.CodeGeneration.AVX.Common
import Core.IndexExpression
import Core.Statement
import Dummies hiding (a, b, c, d, e, f)
import OptimizationGroups.Level1
import TestUtils
import Transformations.Blocking

allMicroBlockedAVXTests =
  TestLabel "allMicroBlockedAVXTests" $
  TestList microBlockedAVXCases

microBlockedAVXCases =
  [ltc "matrix add no remainder" stmtsToAVX maddOpts simpleMAdd,
   ltc "matrix add with remainder" stmtsToAVX maddOpts remainderMAdd]

maddOpts =
  (registerization 4) ++ (tempReduction 4) ++ cleanup ++ (blockingOpts 4)

blockingOpts n =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddN, iConst n),
   (blockMatrixAddM, iConst n)]

simpleMAdd = [matrixAdd c a b]

a = constDblMat "A" 8 8 1 8
b = constDblMat "B" 8 8 1 8
c = constDblMat "C" 8 8 1 8

remainderMAdd = [matrixAdd f d e]

f = constDblMat "A" 13 8 8 1
d = constDblMat "B" 13 8 8 1
e = constDblMat "C" 13 8 8 1

