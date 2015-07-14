module SystemTests.BasicGS(allBasicGSTests) where

import Data.List as L
import Test.HUnit

import CBackEnd.CodeGeneration.AVX.Double
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import CBackEnd.CodeGeneration.Scalar
import Core.IndexExpression
import Core.Matrix
import Core.MemLocation
import Core.Statement
import Dummies hiding (daxpy, alpha, beta, x, y, z)
import Fuzz
import OptimizationGroups.AVXLevel1
import PartitionSearch
import Transformations.Blocking

allBasicGSTests =
  TestLabel "All basic general size tests" $
  TestList $ L.map TestCase
  [assertOptimizationsCorrectGS scalarVarDecls toCStmtsFunction [] (dscal (iVar "m")),
   assertOptimizationsCorrectGS scalarVarDecls toScalarC blockingT (dscal (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (dvadd (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (dvadd2 (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (dscal (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble (avxLvl1Opts 4) (daxpy (iVar "m")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble lv2Opts (dmaddRM (iVar "m") (iVar "n")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble lv2Opts (dgemvRM (iVar "m") (iVar "n")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble lv2Opts (dmmulRM (iVar "m") (iVar "n") (iVar "p")),
   assertOptimizationsCorrectGS avxVarDeclsDouble toAVXDouble lv2Opts (dmaddSetRM (iVar "m") (iVar "m"))]

lv2Opts = (avxLvl1Opts 4) ++ [partitionSearch "b_"]

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

dmaddRM m n =
  let a = matrix "A" m n n (iConst 1) (properties arg double memory)
      b = matrix "B" m n n (iConst 1) (properties arg double memory) in
  [matrixAdd a b b]

dmaddSetRM m n =
  let a = matrix "A" m n n (iConst 1) (properties arg double memory)
      b = matrix "B" m n n (iConst 1) (properties arg double memory)
      c = matrix "C" m n n (iConst 1) (properties arg double memory)
      t = matrix "T" m n n (iConst 1) (properties local double memory) in
  [matrixAdd t a b, matrixSet c t]

daxpy i =
  let alpha = constDblMat "alpha" 1 1 1 1
      y = matrix "y" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      t = matrix "t" i (iConst 1) (iConst 1) (iConst 1) (properties local double memory)
      x = matrix "x" i (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [scalarMultiply t alpha x, matrixAdd y t y]

dgemvRM m n =
  let alpha = constDblMat "alpha" 1 1 1 1
      beta = constDblMat "beta" 1 1 1 1
      y = matrix "y" m (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      t1 = matrix "t" m (iConst 1) (iConst 1) (iConst 1) (properties local double memory)
      t2 = matrix "t" m (iConst 1) (iConst 1) (iConst 1) (properties local double memory)
      x = matrix "x" n (iConst 1) (iConst 1) (iConst 1) (properties arg double memory)
      a = matrix "A" m n n (iConst 1) (properties arg double memory) in
  [setZero t1, matrixMultiply t1 a x, scalarMultiply t2 alpha t1, scalarMultiply y beta y, matrixAdd y t2 y]

dmmulRM m n p =
  let c = matrix "C" m n n (iConst 1) (properties arg double memory)
      a = matrix "A" m p p (iConst 1) (properties arg double memory)
      b = matrix "B" p n n (iConst 1) (properties arg double memory) in
  [matrixMultiply c a b]
