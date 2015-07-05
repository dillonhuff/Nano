module SystemTests.SplitTemps(allSplitTempsTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX
import CompactTemps
import Dummies
import InterchangeAndFuse
import Fusion
import Fuzz
import IndexExpression
import LoopInvariantCodeMotion
import Operations
import Registerization
import RegisterizeTemps
import SMulToBroadcast
import SplitTemps
import Statement
import TestUtils

allSplitTempsTests =
  TestLabel "All split temps tests" $
  TestList renameTempsCases

renameTempsCases =
  [ltc "daxpy 19" avxVarDecls toAVX avxOptsDAXPY (daxpy 19),
   ltc "smul add" avxVarDecls toAVX avxOptsSMulAdd [scalarMultiply tr9c9 alpha m3, matrixAdd m1 tr9c9 m1],
   ltc "fused smul add" avxVarDecls toAVX avxOptsSMulAddFuse [scalarMultiply tr9c9 alpha m3, matrixAdd m1 tr9c9 m1]]

avxOptsSMulAddFuse =
  (registerizeBelow 4 "k_"):
  (registerize 4 "r_"):
  (smulToBroadcast 1 "sm"):
  (registerizeTempsBelow 4):
  (registerizeTemps 4):
  compactTemps:
  (splitTemps "t_"):
  ([interchangeAndFuse] ++ (L.intersperse interchangeAndFuse avxBlockingSMulAdd) ++ [interchangeAndFuse])

avxOptsDAXPY = (registerizeBelow 4 "k_"):(registerize 4 "r_"):(smulToBroadcast 1 "sm"):(registerizeTempsBelow 4):(registerizeTemps 4):compactTemps:(splitTemps "t_"):avxBlockingDAXPY

avxOptsSMulAdd = (registerizeBelow 4 "k_"):(registerize 4 "r_"):(smulToBroadcast 1 "sm"):(registerizeTempsBelow 4):(registerizeTemps 4):compactTemps:(splitTemps "t_"):avxBlockingSMulAdd

avxBlockingDAXPY =
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 4),
   blockScalarMultiplyM (iVar "i2") (iConst 4)]

avxBlockingSMulAdd =
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddN (iVar "i3") (iConst 1),
   blockScalarMultiplyN (iVar "i4") (iConst 1),
   blockMatrixAddM (iVar "i1") (iConst 4),
   blockScalarMultiplyM (iVar "i2") (iConst 4)]

m1 = constDblMat "m1" 9 9 1 9
m2 = constDblMat "m2" 9 9 1 9
m3 = constDblMat "m3" 9 9 1 9