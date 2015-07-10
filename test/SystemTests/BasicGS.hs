module SystemTests.BasicGS(allBasicGSTests) where

import Test.HUnit

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import Dummies
import Fuzz
import IndexExpression
import Matrix
import Statement

allBasicGSTests =
  TestLabel "All basic general size tests" $
  TestCase $ assertOptimizationsCorrectGS scalarVarDecls toCStmtsFunction [] (dscal (iVar "n"))

dscal i =
  let alpha = constDblMat "alpha" 1 1 1 1
      x = matrix "x" (iVar "n") (iConst 1) (iConst 1) (iConst 1) (properties arg double memory) in
  [scalarMultiply x alpha x]
