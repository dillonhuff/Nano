module SystemTests.BasicGS(allBasicGSTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import CBackEnd.CodeGeneration.Scalar
import Dummies
import Fuzz
import IndexExpression
import Matrix
import Statement

allBasicGSTests =
  TestLabel "All basic general size tests" $
  TestList $ L.map TestCase
  [assertOptimizationsCorrectGS scalarVarDecls toCStmtsFunction [] (dscal (iVar "n")),
   assertOptimizationsCorrectGS scalarVarDecls toScalarC blockingT (dscal (iVar "m"))]

blockingT =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 1)]

dscal i =
  let alpha = constDblMat "alpha" 1 1 1 1
      x = matrix "x" (iVar "n") (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [scalarMultiply x alpha x]
