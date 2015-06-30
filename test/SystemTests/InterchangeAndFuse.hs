module SystemTests.InterchangeAndFuse(allInterchangeAndFuseTests) where

import Data.List as L
import Test.HUnit

import Blocking
import Dummies
import Fuzz
import IndexExpression
import InterchangeAndFuse
import Statement

allInterchangeAndFuseTests = TestLabel "All interchange and fuse fuzz tests" $
                           TestList $ L.map (\op -> TestCase $ assertOptimizationsCorrect fusionOpts op) operations
operations =
  [[matrixAdd a b c],
   [matrixAdd c c c, matrixAdd a a a],
   [scalarMultiply a alpha a, matrixMultiply c a b],
   [scalarMultiply a alpha a, matrixAdd tr9c9 a tr9c9, matrixMultiply c tr9c9 b]]

fusionOpts = interchangeAndFuse:blockingOpts

blockingOpts =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 2),
   blockMatrixMultiplyM (iVar "i2") (iConst 2),
   blockMatrixAddM (iVar "i3") (iConst 2)]
