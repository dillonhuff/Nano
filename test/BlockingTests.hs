module BlockingTests(allBlockingTests) where

import Blocking
import IndexExpression
import Matrix
import Module
import Statement

allBlockingTests = do
  testFunction (blockMatrixAddMTest $ iVar "i") blockMatrixAddMCases
  testFunction (blockMatrixAddNTest $ iVar "j") blockMatrixAddNCases
  
  testFunction (blockScalarMultiplyMTest $ iVar "i") blockScalarMultiplyMCases
  testFunction (blockScalarMultiplyNTest $ iVar "j") blockScalarMultiplyNCases
  
  testFunction (blockMatrixMultiplyMTest $ iVar "i") blockMatrixMultiplyMCases
  testFunction (blockMatrixMultiplyNTest $ iVar "j") blockMatrixMultiplyNCases
  testFunction (blockMatrixMultiplyPTest $ iVar "k") blockMatrixMultiplyPCases

blockMatrixAddMCases =
  [((iConst 5, matrixMultiply a a a), [matrixMultiply a a a]),
   ((iConst 4, matrixAdd a a a), [loop "i" (iConst 0) (iConst 4) (iConst 5) [maddABlk4M], maddABlk4ResidualM]),
   ((iConst 1, matrixAdd a a a), [loop "i" (iConst 0) (iConst 1) (iConst 8) [maddBlk1M]])]

blockMatrixAddNCases =
  [((iConst 3, matrixMultiply a a a), [matrixMultiply a a a]),
   ((iConst 1, matrixAdd a a a), [loop "j" (iConst 0) (iConst 1) (iConst 8) [maddABlk1N]])]

blockScalarMultiplyMCases =
  [((iConst 124, matrixAdd a a a), [matrixAdd a a a]),
   ((iConst 3, scalarMultiply a alpha a), [loop "i" (iConst 0) (iConst 3) (iConst 6) [smulABlk3M]]),
   ((iConst 6, scalarMultiply a alpha a), [loop "i" (iConst 0) (iConst 6) (iConst 3) [smulABlk6M], smulABlk6MResidual])]

blockScalarMultiplyNCases =
  [((iConst 3, matrixAdd a a a), [matrixAdd a a a]),
   ((iConst 2, scalarMultiply a alpha a), [loop "j" (iConst 0) (iConst 2) (iConst 7) [smulABlk2N], smulABlk2NResidual])]

blockMatrixMultiplyMCases =
  [((iConst 3, matrixAdd a a a), [matrixAdd a a a]),
   ((iConst 2, matrixMultiply c a b), [loop "i" (iConst 0) (iConst 2) (iConst 7) [mmulCBlk2M], mmulCBlk2MResidual])]

blockMatrixMultiplyNCases =
  [((iConst 3, matrixAdd a a a), [matrixAdd a a a])]

blockMatrixMultiplyPCases =
  [((iConst 3, matrixAdd a a a), [matrixAdd a a a])]

blockMatrixAddMTest iVar (blkFactor, stmt) =
  blockMatrixAddM iVar blkFactor stmt

blockMatrixAddNTest iVar (blkFactor, stmt) =
  blockMatrixAddN iVar blkFactor stmt

blockScalarMultiplyMTest iVar (blkFactor, stmt) =
  blockScalarMultiplyM iVar blkFactor stmt

blockScalarMultiplyNTest iVar (blkFactor, stmt) =
  blockScalarMultiplyN iVar blkFactor stmt

blockMatrixMultiplyMTest iVar (blkFactor, stmt) =
  blockMatrixMultiplyM iVar blkFactor stmt

blockMatrixMultiplyNTest iVar (blkFactor, stmt) =
  blockMatrixMultiplyN iVar blkFactor stmt

blockMatrixMultiplyPTest iVar (blkFactor, stmt) =
  blockMatrixMultiplyP iVar blkFactor stmt

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 13 13 1
c = constDblMat "C" 9 13 1 9

alpha = constDblMat "alpha" 1 1 1 1

maddBlk1M =
  matrixAdd mainA1M mainA1M mainA1M  
maddABlk4M =
  matrixAdd mainA4M mainA4M mainA4M

maddABlk4ResidualM =
  matrixAdd residualA4M residualA4M residualA4M

maddABlk1N =
  matrixAdd mainA1N mainA1N mainA1N

smulABlk3M =
  scalarMultiply mainA3M alpha mainA3M
smulABlk6M =
  scalarMultiply mainA6M alpha mainA6M

smulABlk6MResidual =
  scalarMultiply residualA6M alpha residualA6M

smulABlk2N =
  scalarMultiply mainA2N alpha mainA2N

smulABlk2NResidual =
  scalarMultiply residualA2N alpha residualA2N

mainA1M = subMatrix (iVar "i") (iConst 1) (iConst 0) (iConst 9) a
mainA2M = subMatrix (iVar "i") (iConst 2) (iConst 0) (iConst 9) a
mainA3M = subMatrix (iVar "i") (iConst 3) (iConst 0) (iConst 9) a
mainA4M = subMatrix (iVar "i") (iConst 4) (iConst 0) (iConst 9) a
mainA6M = subMatrix (iVar "i") (iConst 6) (iConst 0) (iConst 9) a

residualA2M = subMatrix (iConst 8) (iConst 1) (iConst 0) (iConst 9) a
residualA4M = subMatrix (iConst 8) (iConst 1) (iConst 0) (iConst 9) a
residualA6M = subMatrix (iConst 6) (iConst 3) (iConst 0) (iConst 9) a

mainA1N = subMatrix (iConst 0) (iConst 9) (iVar "j") (iConst 1) a
mainA2N = subMatrix (iConst 0) (iConst 9) (iVar "j") (iConst 2) a

residualA2N = subMatrix (iConst 0) (iConst 9) (iConst 8) (iConst 1) a

mainC2M = subMatrix (iVar "i") (iConst 2) (iConst 0) (iConst 13) c

residualC2M = subMatrix (iConst 8) (iConst 1) (iConst 0) (iConst 13) c

mmulCBlk2M =
  matrixMultiply mainC2M mainA2M b

mmulCBlk2MResidual =
  matrixMultiply residualC2M residualA2M b

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)
