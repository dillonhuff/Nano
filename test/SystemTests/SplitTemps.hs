module SystemTests.SplitTemps(allSplitTempsTests) where

import Data.List as L
import Test.HUnit

import Transformations.Blocking
import CBackEnd.CodeGeneration.AVX.Double
import Transformations.CompactTemps
import Dummies
import Transformations.InterchangeAndFuse
import Transformations.Fusion
import Fuzz
import Core.IndexExpression
import Transformations.LoopInvariantCodeMotion
import Operations
import Transformations.Registerization
import Transformations.RegisterizeTemps
import Transformations.SMulToBroadcast
import Transformations.SplitTemps
import Core.Statement
import TestUtils

allSplitTempsTests =
  TestLabel "All split temps tests" $
  TestList renameTempsCases

renameTempsCases =
  [ltc "daxpy 19" avxVarDeclsDouble toAVXDouble avxOptsDAXPY (daxpy 19),
   ltc "smul add" avxVarDeclsDouble toAVXDouble avxOptsSMulAdd [scalarMultiply tr9c9 alpha m3, matrixAdd m1 tr9c9 m1],
   ltc "fused smul add" avxVarDeclsDouble toAVXDouble avxOptsSMulAddFuse [scalarMultiply tr9c9 alpha m3, matrixAdd m1 tr9c9 m1]]

avxOptsSMulAddFuse =
  (registerizeBelow 4 "k_"):
  (registerize 4 "r_"):
  (smulToBroadcast "sm"):
  (registerizeTempsBelow 4):
  (registerizeTemps 4):
  compactTemps:
  (splitTemps "t_"):
  ([interchangeAndFuse] ++ (L.intersperse interchangeAndFuse avxBlockingSMulAdd) ++ [interchangeAndFuse])

avxOptsDAXPY = (registerizeBelow 4 "k_"):(registerize 4 "r_"):(smulToBroadcast "sm"):(registerizeTempsBelow 4):(registerizeTemps 4):compactTemps:(splitTemps "t_"):avxBlockingDAXPY

avxOptsSMulAdd = (registerizeBelow 4 "k_"):(registerize 4 "r_"):(smulToBroadcast "sm"):(registerizeTempsBelow 4):(registerizeTemps 4):compactTemps:(splitTemps "t_"):avxBlockingSMulAdd

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
