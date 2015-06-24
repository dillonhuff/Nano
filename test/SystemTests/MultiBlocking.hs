module SystemTests.MultiBlocking(allMultiBlockingTests) where

import Data.List as L
import Control.Monad.Random
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import Dummies
import IndexExpression
import Statement

allMultiBlockingTests = TestLabel "All multi blocking tests" $
                        TestList $ L.map (\op -> TestCase $ assertRandomTransformsCorrect op) testOperations

blockingTransforms =
  [blockMatrixAddM (iVar "i1") (iConst 1),
   blockMatrixAddN (iVar "i2") (iConst 1),
   blockMatrixAddM (iVar "i3") (iConst 3),
   blockMatrixAddN (iVar "i4") (iConst 11),
   blockScalarMultiplyM (iVar "i5") (iConst 1),
   blockScalarMultiplyN (iVar "i6") (iConst 3),
   blockScalarMultiplyN (iVar "i7") (iConst 4),
   blockMatrixMultiplyM (iVar "i8") (iConst 1),
   blockMatrixMultiplyN (iVar "i9") (iConst 1),
   blockMatrixMultiplyP (iVar "i10") (iConst 1)]
   

testOperations =
   [[matrixAdd f g h],
    [matrixAdd f f f],
    [matrixAdd g h f],
    [matrixAdd a b c],
    [scalarMultiply a alpha a],
    [matrixMultiply a b c],
    [matrixMultiply a b b],
    [matrixMultiply h d e]]

randomBit :: (RandomGen g) => Rand g Int
randomBit = getRandomR (0, 1)

randomBits :: (RandomGen g) => Int -> Rand g [Int]
randomBits n = sequence (replicate n randomBit)

selectTransformsR :: (RandomGen g) => [a] -> Rand g [a]
selectTransformsR ts = do
  selectList <- randomBits (length ts)
  return $ L.map fst $ L.filter (\(t, b) -> b == 1) $ L.zip ts selectList

selectTransforms ts = evalRandIO (selectTransformsR ts)

assertRandomTransformsCorrect operation = do
  transformsToApply <- selectTransforms blockingTransforms
  let (transformedOp, _) = operationToC "transformedOp" $ applyTransforms transformsToApply operation
      (regularOp, argInfo) = operationToC "op" operation in
    do
      scRes <- runSanityCheck "multiBlockingTest" regularOp transformedOp argInfo
      assertEqual (failMessageInfo transformedOp regularOp argInfo) scRes "true\n"

failMessageInfo resultOp regularOp argInfo =
  "Operation:\n" ++ (prettyPrint 0 regularOp) ++ "\n" ++
  "Arguments:\n" ++ show argInfo ++ "\n" ++
  "Resulting Operation:\n" ++ (prettyPrint 0 resultOp) ++ "\n"

applyTransforms :: [Statement -> [Statement]] -> [Statement] -> [Statement]
applyTransforms [] op = op
applyTransforms (t:rest) op = expandStatementsBU t $ applyTransforms rest op
  
