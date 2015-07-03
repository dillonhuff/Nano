module SystemTests.AVXCodeGen(allAVXCodeGenTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX
import Dummies
import Fuzz
import IndexExpression
import Scalarization
import Statement
import TestUtils

allAVXCodeGenTests =
  TestLabel "All AVX code generation tests" $
  TestList $ avxTestCases

avxTestCases =
  [ltc "vector add" avxVarDecls toAVX avxOpts [matrixAdd x y z]]

avxOpts = (scalarize 8 "r_"):avxBlocking

avxBlocking =
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 8)]
