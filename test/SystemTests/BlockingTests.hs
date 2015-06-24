module SystemTests.BlockingTests(allSystemBlockingTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.SanityCheck
import CBackEnd.SanityCheckHarness
import Dummies
import IndexExpression
import Matrix
import Module
import Statement

allSystemBlockingTests =
  TestLabel "Single blocking tests" $ TestList
            [testBlockMAdd,
             testBlockSMul,
             testBlockTrans,
             testBlockMMul]
  
testBlockMAdd = TestLabel "Single matrix add blocking" $ TestList
  [makeTestCasesIO (testBlocking blockMatrixAddM) blockMAddCases,
   makeTestCasesIO (testBlocking blockMatrixAddN) blockMAddCases]

testBlockSMul = TestLabel "Single scalar multiply blocking" $ TestList
  [makeTestCasesIO (testBlocking blockScalarMultiplyM) blockSMulCases,
   makeTestCasesIO (testBlocking blockScalarMultiplyN) blockSMulCases]

testBlockTrans = TestLabel "Single transpose multiply blocking" $ TestList
  [makeTestCasesIO (testBlocking blockTransposeM) blockTransCases,
   makeTestCasesIO (testBlocking blockTransposeN) blockTransCases]

testBlockMMul = TestLabel "Single matrix multiply blocking" $ TestList
  [makeTestCasesIO (testBlocking blockMatrixMultiplyM) blockMMulCases,
   makeTestCasesIO (testBlocking blockMatrixMultiplyN) blockMMulCases,
   makeTestCasesIO (testBlocking blockMatrixMultiplyP) blockMMulCases]

blockMAddCases =
  L.map (\x -> (x, True))
  [(iConst 1, maddCBA),
   (iConst 2, maddCBA),
   (iConst 3, maddCBA),
   (iConst 4, maddCBA),
   (iConst 5, maddCBA),
   (iConst 6, matrixAdd h f g),
   (iConst 7, matrixAdd g f g),
   (iConst 8, matrixAdd g f f),
   (iConst 9, matrixAdd g f h),
   (iConst 10, matrixAdd f g g)]

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

blockTransCases =
  L.map (\x -> (x, True))
  [(iConst 1, matrixTranspose h i),
   (iConst 13, matrixTranspose h i),
   (iConst 1, matrixTranspose i h),
   (iConst 5, matrixTranspose i h)]

testBlocking blkFunc (blkFactor, stmt) =
  let blockedStmt = blkFunc (iVar "i") blkFactor stmt
      (cBlocked, _) = operationToC "blocked" blockedStmt
      (cUnBlocked, unBlockedArgs) = operationToC "unblocked" [stmt] in
  do
    scRes <- runSanityCheck "blockingTest" cUnBlocked cBlocked unBlockedArgs
    return $ scRes == "true\n"

