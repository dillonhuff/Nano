module SystemTests.AVXCodeGen(allAVXCodeGenTests) where

import Data.List as L
import Test.HUnit

import Transformations.BlockDot
import Transformations.Blocking
import CBackEnd.CodeGeneration.AVX.Common
import Transformations.CompactTemps
import Dummies
import Transformations.Fusion
import Fuzz
import Core.IndexExpression
import Transformations.LoopInvariantCodeMotion
import Operations
import Transformations.IntroducePacking
import Transformations.RegisterizeTemps
import Transformations.SMulToBroadcast
import Transformations.SplitTemps
import Core.Statement
import TestUtils

allAVXCodeGenTests =
  TestLabel "All AVX code generation tests" $
  TestList $ avxTestCases

avxTestCases =
  [ltc "vector add" stmtsToAVX avxOpts [matrixAdd x y z],
   ltc "vector smul" stmtsToAVX avxOpts [scalarMultiply x alpha x],
   ltc "daxpy" stmtsToAVX avxOpts (daxpy 16),
   ltc "matrix add even" stmtsToAVX avxOptsSMulAdd [matrixAdd m1 m2 m3],
   ltc "matrix add uneven" stmtsToAVX avxOptsSMulAdd [matrixAdd n1 n2 n3],
   ltc "dot product" stmtsToAVX avxOptsSMulAdd [matrixMultiply alpha x1 y1],
   ltc "uneven size dot product" stmtsToAVX avxOptsSMulAdd [matrixMultiply alpha xu1 yu1]]

avxOpts = pullCodeOutOfLoops:(registerizeTemps 4):(pack 4 "r_"):(smulToBroadcast "sm"):compactTemps:fuseInnerLoops:avxBlocking

avxBlocking =
  L.intersperse fuseInnerLoops $
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 4),
   blockScalarMultiplyM (iVar "i2") (iConst 4)]

avxOptsSMulAdd = (registerizeTemps 4):(pack 4 "r_"):(smulToBroadcast "sm"):(blockDot 4 "d_"):compactTemps:(splitTemps "t_"):avxBlockingSMulAdd

avxBlockingSMulAdd =
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddN (iVar "i3") (iConst 4),
   blockScalarMultiplyN (iVar "i4") (iConst 4),
   blockMatrixAddM (iVar "i1") (iConst 1),
   blockScalarMultiplyM (iVar "i2") (iConst 1)]

m1 = constDblMat "M1" 8 8 8 1
m2 = constDblMat "M2" 8 8 8 1
m3 = constDblMat "M3" 8 8 8 1

n1 = constDblMat "N1" 9 9 9 1
n2 = constDblMat "N2" 9 9 9 1
n3 = constDblMat "N3" 9 9 9 1

x1 = constDblMat "x" 1 8 1 1
y1 = constDblMat "y" 8 1 1 1

xu1 = constDblMat "x" 1 11 1 1
yu1 = constDblMat "y" 11 1 1 1
