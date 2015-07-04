module SystemTests.AVXCodeGen(allAVXCodeGenTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import LoopInvariantCodeMotion
import Operations
import Registerization
import RegisterizeTemps
import SMulToBroadcast
import Statement
import TestUtils

allAVXCodeGenTests =
  TestLabel "All AVX code generation tests" $
  TestList $ avxTestCases

avxTestCases =
  [ltc "vector add" avxVarDecls toAVX avxOpts [matrixAdd x y z],
   ltc "vector smul" avxVarDecls toAVX avxOpts [scalarMultiply x alpha x],
   ltc "daxpy" avxVarDecls toAVX avxOpts (daxpy 16)]

avxOpts = pullCodeOutOfLoops:(registerize 4 "r_"):(smulToBroadcast 1 "sm"):(registerizeTemps 4):compactTemps:fuseInnerLoops:avxBlocking

avxBlocking =
  L.intersperse fuseInnerLoops $
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 4),
   blockScalarMultiplyM (iVar "i2") (iConst 4)]
