module SystemTests.SplitTemps(allSplitTempsTests) where

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
import SplitTemps
import Statement
import TestUtils

allSplitTempsTests =
  TestLabel "All rename temps tests" $
  TestList renameTempsCases

renameTempsCases =
  [ltc "daxpy 19" avxVarDecls toAVX avxOpts (daxpy 19)]

avxOpts = (registerizeBelow 4 "k_"):(registerize 4 "r_"):(smulToBroadcast 1 "sm"):(registerizeTempsBelow 4):(registerizeTemps 4):compactTemps:(splitTemps "t_"):avxBlocking

avxBlocking =
  L.intersperse fuseInnerLoops $
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 4),
   blockScalarMultiplyM (iVar "i2") (iConst 4)]
