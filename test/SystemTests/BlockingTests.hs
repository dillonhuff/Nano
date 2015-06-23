module SystemTests.BlockingTests(allSystemBlockingTests) where

import Data.List as L

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.SanityCheck
import CBackEnd.SanityCheckHarness
import IndexExpression
import Matrix
import Module
import Statement

allSystemBlockingTests = do
  testFunctionIO (testBlocking blockMatrixAddM) blockAddMCases

blockAddMCases =
  L.map (\x -> (x, True))
  [(iConst 1, maddCBA),
   (iConst 3, maddCBA),
   (iConst 9, maddCBA)]

testBlocking blkFunc (blkFactor, stmt) =
  let blockedStmt = blkFunc (iVar "i") blkFactor stmt
      (cBlocked, _) = operationToC "blocked" blockedStmt
      (cUnBlocked, unBlockedArgs) = operationToC "unblocked" [stmt] in
  do
    scRes <- runSanityCheck "blockingTest" cUnBlocked cBlocked unBlockedArgs
    return $ scRes == "true\n"

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 9 1 9
c = constDblMat "C" 9 9 1 9

maddCBA = matrixAdd c b a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

