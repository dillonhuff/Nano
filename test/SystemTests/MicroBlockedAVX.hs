module SystemTests.MicroBlockedAVX(allMicroBlockedAVXTests) where

import Data.List as L
import Test.HUnit

import CBackEnd.CodeGeneration.AVX.Common
import Core.IndexExpression
import Core.Statement
import Dummies hiding (a, b, c)
import OptimizationGroups.Level1
import TestUtils
import Transformations.Blocking

allMicroBlockedAVXTests =
  TestLabel "allMicroBlockedAVXTests" $
  TestList microBlockedAVXCases

microBlockedAVXCases =
  [ltc "matrix add unmatched strides" stmtsToAVX maddUnmatchedOpts maddUnmatched]

maddUnmatchedOpts =
  (registerization 4) ++ (tempReduction 4) ++ cleanup ++ (blockingOpts 4)

blockingOpts n =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddN, iConst n),
   (blockMatrixAddM, iConst n)]

maddUnmatched = [matrixAdd c a b]

a = constDblMat "A" 8 8 1 8
b = constDblMat "B" 8 8 8 1
c = constDblMat "C" 8 8 1 8

