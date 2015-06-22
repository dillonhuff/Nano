module StatementTests() where

import IndexExpression
import Matrix
import Module
import Statement

allStatementTests = do
  testFunction (blockMatrixAddMTest $ iVar "i") blockMatrixAddMCases

blockMatrixAddMCases =
  [((iConst 5, matrixMultiply a a a), [matrixMultiply a a a]),
   ((iConst 4, matrixAdd a a a), [loop "i" (iConst 0) (iConst 4) (iConst 5) [maddABlk4], maddABlk4Residual])]

blockMatrixAddMTest iVar (blkFactor, stmt) =
  blockMatrixAddM iVar blkFactor stmt

maddABlk4 =
  matrixAdd mainA4 mainA4 mainA4

maddABlk4Residual =
  matrixAdd residualA4 residualA4 residualA4
  
a = constDblMat "A" 9 9 1 9

mainA4 = subMatrix (iVar "i") (iConst 4) (iConst 0) (iConst 9) a
residualA4 = subMatrix (iConst 8) (iConst 1) (iConst 0) (iConst 9) a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)
