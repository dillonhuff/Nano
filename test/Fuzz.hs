module Fuzz(applyRandomOptimizations,
            applyOptimizations,
            selectTransforms,
            assertOptimizationsCorrect,
            assertRandomOptimizationsCorrect) where

import Control.Monad.Random
import Data.List as L
import System.Random.Shuffle
import Test.HUnit

import CBackEnd.CodeGeneration
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import Statement

applyTransforms :: [Statement -> [Statement]] -> [Statement] -> [Statement]
applyTransforms [] op = op
applyTransforms (t:rest) op = expandStatementsBU t $ applyTransforms rest op
  
randomBit :: (RandomGen g) => Rand g Int
randomBit = getRandomR (0, 1)

randomBits :: (RandomGen g) => Int -> Rand g [Int]
randomBits n = sequence (replicate n randomBit)

selectTransformsR :: (RandomGen g) => [a] -> Rand g [a]
selectTransformsR ts = do
  selectList <- randomBits (length ts)
  let selected = L.map fst $ L.filter (\(t, b) -> b == 1) $ L.zip ts selectList in
    shuffleM selected

selectTransforms ts = evalRandIO (selectTransformsR ts)

applyRandomOptimizations :: [[Statement] -> [Statement]] -> [Statement] -> IO [Statement]
applyRandomOptimizations possibleOptimizations stmts = do
  toApply <- selectTransforms possibleOptimizations
  return $ applyOptimizations toApply stmts

applyOptimizations :: [[Statement] -> [Statement]] -> [Statement] -> [Statement]
applyOptimizations [] stmts = stmts
applyOptimizations (r:rest) stmts = r $ applyOptimizations rest stmts

assertRandomOptimizationsCorrect possibleOptimizations operation = do
  transformsToApply <- selectTransforms possibleOptimizations
  assertOptimizationsCorrect transformsToApply operation

assertOptimizationsCorrect transformsToApply operation =
  let (transformedOp, _) = operationToC "transformedOp" $ applyOptimizations transformsToApply operation
      (regularOp, argInfo) = operationToC "op" operation in
    do
      scRes <- runSanityCheck "fuzzTest" regularOp transformedOp argInfo
      assertEqual (failMessageInfo transformedOp regularOp argInfo) scRes "true\n"

failMessageInfo resultOp regularOp argInfo =
  "Operation:\n" ++ (prettyPrint 0 regularOp) ++ "\n" ++
  "Arguments:\n" ++ show argInfo ++ "\n" ++
  "Resulting Operation:\n" ++ (prettyPrint 0 resultOp) ++ "\n"
