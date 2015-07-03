module SystemTests.Scalarization(allScalarizationTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Scalar
import Dummies
import Fuzz
import IndexExpression
import Scalarization
import Statement

allScalarizationTests = TestLabel "All scalarization system tests" $
                      TestList $ L.map (\op -> TestCase $ assertOptimizationsCorrect scalarVarDecls toScalarC scalarizationOpts op) compoundTestOperations

scalarizationOpts = (scalarize "r_"):blockingOpts

blockingOpts = 
  L.map (\t -> expandStatementsBU t)
  [blockMatrixMultiplyM (iVar "i1") (iConst 1),
   blockMatrixMultiplyN (iVar "i2") (iConst 1),
   blockMatrixMultiplyP (iVar "i3") (iConst 1),
   blockMatrixAddM (iVar "i4") (iConst 1),
   blockMatrixAddN (iVar "i5") (iConst 1),
   blockScalarMultiplyM (iVar "i6") (iConst 1),
   blockScalarMultiplyN (iVar "i7") (iConst 1),
   blockMatrixTransposeM (iVar "i8") (iConst 1),
   blockMatrixTransposeN (iVar "i9") (iConst 1)]
