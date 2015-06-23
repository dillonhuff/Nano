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
  testBlockMAdd
  testBlockSMul
  testBlockMMul
  
testBlockMAdd = do
  testFunctionIO (testBlocking blockMatrixAddM) blockMAddCases
  testFunctionIO (testBlocking blockMatrixAddN) blockMAddCases

testBlockSMul = do
  testFunctionIO (testBlocking blockScalarMultiplyM) blockSMulCases
  testFunctionIO (testBlocking blockScalarMultiplyN) blockSMulCases

testBlockMMul = do
  testFunctionIO (testBlocking blockMatrixMultiplyM) blockMMulCases
  testFunctionIO (testBlocking blockMatrixMultiplyN) blockMMulCases
  testFunctionIO (testBlocking blockMatrixMultiplyP) blockMMulCases

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

blockMMulCases =
  L.map (\x -> (x, True))
  [(iConst 1, matrixMultiply c b a),
   (iConst 2, matrixMultiply c a a),
   (iConst 3, matrixMultiply f d e),
   (iConst 4, mmulCBA),
   (iConst 5, mmulCBA),
   (iConst 6, mmulCBA),
   (iConst 7, mmulCBA),
   (iConst 8, mmulCBA),
   (iConst 9, mmulCBA),
   (iConst 10, mmulCBA)]

testBlocking blkFunc (blkFactor, stmt) =
  let blockedStmt = blkFunc (iVar "i") blkFactor stmt
      (cBlocked, _) = operationToC "blocked" blockedStmt
      (cUnBlocked, unBlockedArgs) = operationToC "unblocked" [stmt] in
  do
    scRes <- runSanityCheck "blockingTest" cUnBlocked cBlocked unBlockedArgs
    return $ scRes == "true\n"

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 9 7 9
c = constDblMat "C" 9 9 9 1
d = constDblMat "D" 13 9 1 13
e = constDblMat "E" 9 4 4 19
f = constDblMat "F" 13 4 4 1

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a
mmulCBA = matrixMultiply c b a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

