module SystemTests.Scalarization(allScalarizationTests) where

import Data.List as L
import Test.HUnit

import Blocking
import Dummies
import Fuzz
import IndexExpression
import Scalarization
import Statement

allScalarizationTests = TestLabel "All scalarization system tests" $
                      TestList $ L.map (\op -> TestCase $ assertOptimizationsCorrect scalarizationOpts op) [[matrixAdd a b c]] --compoundTestOperations

scalarizationOpts = (scalarize "r_"):blockingOpts

blockingOpts = 
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 1),
   blockMatrixMultiplyM (iVar "i2") (iConst 1),
   blockMatrixAddM (iVar "i3") (iConst 1),
   blockMatrixAddN (iVar "i4") (iConst 1)]
