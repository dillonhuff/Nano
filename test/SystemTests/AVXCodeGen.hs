module SystemTests.AVXCodeGen(allAVXCodeGenTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX
import Dummies
import Fuzz
import IndexExpression
import LoopInvariantCodeMotion
import Scalarization
import SMulToBroadcast
import Statement
import TestUtils

allAVXCodeGenTests =
  TestLabel "All AVX code generation tests" $
  TestList $ avxTestCases

avxTestCases =
  [ltc "vector add" avxVarDecls toAVX avxOpts [matrixAdd x y z],
   ltc "vector smul" avxVarDecls toAVX avxOpts [scalarMultiply x alpha x]]

avxOpts = pullCodeOutOfLoops:(scalarize 4 "r_"):(smulToBroadcast 1 "sm"):avxBlocking

avxBlocking =
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 4),
   blockScalarMultiplyM (iVar "i2") (iConst 4)]
