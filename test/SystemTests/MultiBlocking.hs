module SystemTests.MultiBlocking(allMultiBlockingTests) where

import Data.List as L
import Control.Monad.Random
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import Dummies
import Fuzz
import IndexExpression
import Statement

allMultiBlockingTests = TestLabel "All multi blocking tests" $
                        TestList $ L.map (\op -> TestCase $ assertRandomTransformsCorrect op) testOperations

assertRandomTransformsCorrect operation = do
  transformsToApply <- selectTransforms blockingOptimizations
  let (transformedOp, _) = operationToC "transformedOp" $ applyOptimizations transformsToApply operation
      (regularOp, argInfo) = operationToC "op" operation in
    do
      scRes <- runSanityCheck "multiBlockingTest" regularOp transformedOp argInfo
      assertEqual (failMessageInfo transformedOp regularOp argInfo) scRes "true\n"

failMessageInfo resultOp regularOp argInfo =
  "Operation:\n" ++ (prettyPrint 0 regularOp) ++ "\n" ++
  "Arguments:\n" ++ show argInfo ++ "\n" ++
  "Resulting Operation:\n" ++ (prettyPrint 0 resultOp) ++ "\n"

