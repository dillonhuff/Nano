module SystemTests.BasicGS(allBasicGSTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX.Double
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import CBackEnd.CodeGeneration.Scalar
import Dummies hiding (daxpy)
import Fuzz
import IndexExpression
import Matrix
import OptimizationGroups.AVXLevel1
import Statement

allBasicGSTests =
  TestLabel "All basic general size tests" $
  TestList $ L.map TestCase
  [assertOptimizationsCorrectGS scalarVarDecls toCStmtsFunction [] (dscal (iVar "m")),
   assertOptimizationsCorrectGS scalarVarDecls toScalarC blockingT (dscal (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (dvadd (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (dvadd2 (iVar "m")),
--   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (dscal (iVar "m"))]
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (daxpy (iVar "m"))]

blockingT =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 1)]

dscal i =
  let alpha = constDblMat "alpha" 1 1 1 1
      x = matrix "x" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [scalarMultiply x alpha x]

dvadd2 i =
  let x = matrix "x" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      y = matrix "y" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      z = matrix "z" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [matrixAdd x y y, matrixAdd z y y]

dvadd i =
  let x = matrix "x" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      y = matrix "y" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [matrixAdd x y y]

daxpy i =
  let alpha = constDblMat "alpha" 1 1 1 1
      y = matrix "y" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      t = matrix "t" i (iConst 1) (iConst 1) (iConst 1) (properties local double memory)
      x = matrix "x" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [scalarMultiply t alpha x, matrixAdd y t y]
