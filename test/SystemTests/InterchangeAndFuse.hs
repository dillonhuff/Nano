module SystemTests.InterchangeAndFuse(allInterchangeAndFuseTests) where

import Data.List as L
import Test.HUnit

import Transformations.Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import Dummies
import Fuzz
import Core.IndexExpression
import Transformations.InterchangeAndFuse
import Core.Statement

allInterchangeAndFuseTests = TestLabel "All interchange and fuse system tests" $
                           TestList $ L.map
                                    (\op -> TestCase $ assertOptimizationsCorrect scalarVarDecls stmtsToCFunctions interchangeAndFuseOpts op)
                                    compoundTestOperations

interchangeAndFuseOpts = interchangeAndFuse:blockingOpts

blockingOpts =
  L.map (\t -> expandStatementsBU t)
  [blockScalarMultiplyM (iVar "i1") (iConst 2),
   blockMatrixMultiplyM (iVar "i2") (iConst 2),
   blockMatrixAddM (iVar "i3") (iConst 2)]
