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
                           TestList $ L.map (\op -> TestCase $ assertOptimizationsCorrect fusionOpts op) compoundTestOperations
fusionOpts = interchangeAndFuse:blockingOpts

blockingOpts =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 2),
   blockMatrixMultiplyM (iVar "i2") (iConst 2),
   blockMatrixAddM (iVar "i3") (iConst 2)]
