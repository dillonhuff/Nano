module SystemTests.Fusion(allFusionTests) where

import Data.List as L
import Test.HUnit

import Blocking
import Dummies
import Fusion
import Fuzz
import IndexExpression
import Statement

allFusionTests = TestLabel "All fusion fuzz tests" $
                 TestList $ L.map (\op -> TestCase $ assertRandomOptimizationsCorrect fusionOpts op) operations

operations =
  [[matrixAdd a b c],
   [matrixAdd c c c, matrixAdd a a a],
   [scalarMultiply a alpha a, matrixMultiply c a b],
   [scalarMultiply a alpha a, matrixAdd tr9c9 a tr9c9, matrixMultiply c tr9c9 b]]

fusionOpts = fuseInnerLoops:blockingOpts

blockingOpts =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 3),
   blockMatrixMultiplyM (iVar "i2") (iConst 3),
   blockMatrixAddM (iVar "i3") (iConst 3)]

