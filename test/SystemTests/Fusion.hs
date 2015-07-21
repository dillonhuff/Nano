module SystemTests.Fusion(allFusionTests) where

import Data.List as L
import Test.HUnit

import Transformations.Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import Dummies
import Transformations.Fusion
import Fuzz
import Core.IndexExpression
import Core.Statement

allFusionTests = TestLabel "All fusion fuzz tests" $
                 TestList $ L.map (\op -> TestCase $ assertOptimizationsCorrect stmtsToCFunctions fusionOpts op) compoundTestOperations

fusionOpts = fuseInnerLoops:blockingOpts

blockingOpts =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 3),
   blockMatrixMultiplyM (iVar "i2") (iConst 3),
   blockMatrixAddM (iVar "i3") (iConst 3)]

