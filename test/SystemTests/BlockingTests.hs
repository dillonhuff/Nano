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
  testFunctionIO (testBlocking blockMatrixAddM) blockMAddCases
  testFunctionIO (testBlocking blockMatrixAddN) blockMAddCases
  testFunctionIO (testBlocking blockScalarMultiplyM) blockSMulCases

blockMAddCases =
  L.map (\x -> (x, True))
  [(iConst 1, maddCBA),
   (iConst 2, maddCBA),
   (iConst 3, maddCBA),
   (iConst 4, maddCBA),
   (iConst 5, maddCBA),
   (iConst 6, maddCBA),
   (iConst 7, maddCBA),
   (iConst 8, maddCBA),
   (iConst 9, maddCBA),
   (iConst 10, maddCBA)]

blockSMulCases =
  L.map (\x -> (x, True))
  [(iConst 1, smulCAlphaA),
   (iConst 2, smulCAlphaA),
   (iConst 3, smulCAlphaA),
   (iConst 4, smulCAlphaA),
   (iConst 5, smulCAlphaA),
   (iConst 6, smulCAlphaA),
   (iConst 7, smulCAlphaA),
   (iConst 8, smulCAlphaA),
   (iConst 9, smulCAlphaA),
   (iConst 10, smulCAlphaA)]

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

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

