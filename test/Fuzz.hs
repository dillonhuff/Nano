module Fuzz(applyRandomOptimizations,
            applyOptimizations,
            selectTransforms,
            assertOptimizationsCorrect,
            assertOptimizationsCorrectGS,
            assertRandomOptimizationsCorrect) where

import Control.Monad.Random
import Data.List as L
import System.Random.Shuffle
import Test.HUnit

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Core
import CBackEnd.CodeGeneration.Function
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.Statement
import Utils

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

assertRandomOptimizationsCorrect codeGenFunc possibleOptimizations operation = do
  transformsToApply <- selectTransforms possibleOptimizations
  assertOptimizationsCorrect codeGenFunc transformsToApply operation

assertOptimizationsCorrect :: ([Statement] -> ([(CType, String)], [CStmt String])) ->
                              ([[Statement] -> [Statement]]) ->
                              [Statement] ->
                              IO ()
assertOptimizationsCorrect codeGenFunc transformsToApply operation =
  let (transformedOp, _) = operationToC codeGenFunc "transformedOp" $ applyOptimizations transformsToApply operation
      (regularOp, argInfo) = operationToC stmtsToCFunctions "op" operation in
    do
      scRes <- runSanityCheck "fuzzTest" regularOp transformedOp argInfo
      assertEqual (failMessageInfo transformedOp regularOp argInfo) "true\n" scRes

assertOptimizationsCorrectGS :: ([Statement] -> ([(CType, String)], [CStmt String])) ->
                                ([[Statement] -> [Statement]]) ->
                                [Statement] ->
                                IO ()
assertOptimizationsCorrectGS codeGenFunc transformsToApply operation =
  let (transformedOp, _) = operationToC codeGenFunc "transformedOp" $ applyOptimizations transformsToApply operation
      (regularOp, bufsAndIVars) = operationToC stmtsToCFunctions "op" operation
      argInfo = L.takeWhile (\b -> isCPtr $ bufType b) bufsAndIVars
      indInfo = L.dropWhile (\b -> isCPtr $ bufType b) bufsAndIVars
      indDecls = bufDecls indInfo
      indNames = L.map bufName indInfo
      indVars = L.map cVar indNames
      indInits = L.map (\indVar -> cExprSt (cAssign indVar (cIntLit 79)) "") indVars
      scFuncall = \bufs -> [cExprSt (cFuncall (cFuncName regularOp) ((L.map (cVar . bufName) bufs) ++ indVars)) ""]
      testFuncall = \bufs -> [cExprSt (cFuncall (cFuncName transformedOp) ((L.map (cVar . bufName) bufs) ++ indVars)) ""] in
    do
      scRes <- runSanityCheckGS "gs_fuzzTest" indDecls indInits scFuncall testFuncall regularOp transformedOp argInfo
      assertEqual (failMessageInfo transformedOp regularOp argInfo) "true\n" scRes

failMessageInfo resultOp regularOp argInfo =
  "Operation:\n" ++ (prettyPrint 0 regularOp) ++ "\n" ++
  "Arguments:\n" ++ show argInfo ++ "\n" ++
  "Resulting Operation:\n" ++ (prettyPrint 0 resultOp) ++ "\n"
