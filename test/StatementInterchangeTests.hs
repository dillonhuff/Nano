module StatementInterchangeTests(allStatementInterchangeTests) where

import Data.List as L
import Test.HUnit

import Transformations.Blocking
import Dummies
import IndexExpression
import Module
import Statement
import Transformations.StatementInterchange

allStatementInterchangeTests =
  TestLabel "All statement interchange tests" $ TestList
  [makeTestCases interchangeStmts interchangeCases]

interchangeCases =
  [([matrixAdd fA fB fC], [matrixAdd fA fB fC]),
   (twoMAdds, twoInterchangedMAdds),
   (twoNonInterchangeableMAdds, twoNonInterchangeableMAdds),
   ([maddABBM4Residual, maddCAAM4Main], [maddCAAM4Main, maddABBM4Residual]),
   ([maddABBM4Residual, maddCAAM4Residual], [maddABBM4Residual, maddCAAM4Residual])]

twoMAdds =
  [maddABBM4Residual, maddCAAM4Main]

twoInterchangedMAdds =
  [maddCAAM4Main, maddABBM4Residual]

twoNonInterchangeableMAdds =
  [maddCAAM4Main, maddCAA]

maddABB = matrixAdd fA fB fB
maddCAA = matrixAdd fC fA fA

maddABBM4 = blockMatrixAddM (iVar "i1") (iConst 4) maddABB
maddCAAM4 =  blockMatrixAddM (iVar "i2") (iConst 4) maddCAA

maddABBM4Main = L.head maddABBM4
maddABBM4Residual = L.head $ L.tail maddABBM4

maddCAAM4Main = L.head maddCAAM4
maddCAAM4Residual = L.head $ L.tail maddCAAM4
