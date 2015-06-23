module StatementTests() where

import IndexExpression
import Matrix
import Module
import Statement

allStatementTests = do
  testFunction (blockMatrixAddMTest $ iVar "i") blockMatrixAddMCases
  testFunction (blockMatrixAddNTest $ iVar "j") blockMatrixAddNCases

blockMatrixAddMCases =
  [((iConst 5, matrixMultiply a a a), [matrixMultiply a a a]),
   ((iConst 4, matrixAdd a a a), [loop "i" (iConst 0) (iConst 4) (iConst 5) [maddABlk4M], maddABlk4ResidualM]),
   ((iConst 1, matrixAdd a a a), [loop "i" (iConst 0) (iConst 1) (iConst 8) [maddBlk1M]])]

blockMatrixAddNCases =
  [((iConst 3, matrixMultiply a a a), [matrixMultiply a a a]),
   ((iConst 1, matrixAdd a a a), [loop "j" (iConst 0) (iConst 1) (iConst 8) [maddABlk1N]])]

blockMatrixAddMTest iVar (blkFactor, stmt) =
  blockMatrixAddM iVar blkFactor stmt

blockMatrixAddNTest iVar (blkFactor, stmt) =
  blockMatrixAddN iVar blkFactor stmt

a = constDblMat "A" 9 9 1 9

maddBlk1M =
  matrixAdd mainA1M mainA1M mainA1M
  
maddABlk4M =
  matrixAdd mainA4M mainA4M mainA4M

maddABlk4ResidualM =
  matrixAdd residualA4M residualA4M residualA4M

maddABlk1N =
  matrixAdd mainA1N mainA1N mainA1N

mainA1M = subMatrix (iVar "i") (iConst 1) (iConst 0) (iConst 9) a
mainA4M = subMatrix (iVar "i") (iConst 4) (iConst 0) (iConst 9) a
residualA4M = subMatrix (iConst 8) (iConst 1) (iConst 0) (iConst 9) a

mainA1N = subMatrix (iConst 0) (iConst 9) (iVar "j") (iConst 1) a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)
