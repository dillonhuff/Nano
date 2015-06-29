module Main(main) where

import Data.List as L

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.Syntax
import IndexExpression
import Matrix
import CBackEnd.SanityCheck
import CBackEnd.Timing
import CBackEnd.TimingHarness
import CostModel.Flops
import Dummies
import Fuzz
import Reporting.Report
import Search.Exhaustive
import Statement

{-main :: IO ()
main = do
  scResStr <- runSanityCheck "mainMAddTest" maddOp maddRefinedOp args
  putStrLn $ scResStr
  where
    (maddOp, args) = operationToC "matrixAdd" [plusABC]
    (maddRefinedOp, _) = operationToC "optimizedMatrixAdd" optimizedPlusABC

optimizedPlusABC = blockMatrixAddM (iVar "i") (iConst 8) plusABC

plusABC =
  matrixAdd a b c

a = constDblMat "A" 9 15 1 9
b = constDblMat "B" 9 15 15 1
c = constDblMat "C" 9 15 1 9

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

-}

{-main :: IO ()
main = do
  res <- testBlocking blockScalarMultiplyM (iConst 2, smulCAlphaA)
  putStrLn $ show res

testBlocking blkFunc (blkFactor, stmt) = do
  putStrLn $ show [stmt]
  putStrLn $ show $ argInfoList [stmt]

-}

{-main :: IO ()
main =
  let res = expandStatementsBU (blockMatrixMultiplyP (iVar "k") (iConst 6)) $
            expandStatementsBU (blockMatrixMultiplyM (iVar "j") (iConst 3)) $
            expandStatementsBU (blockMatrixMultiplyN (iVar "i") (iConst 8)) $
            [matrixMultiply c b a]
      (cOp, argInfo) = operationToC "blockedMatrixMul" res in
  do
    res <- runTimingCode "mainTiming" cOp argInfo
    putStrLn res

--    putStrLn $ prettyPrint 0 $ timingHarness "blockedMatrixMul" argInfo

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 9 1 9
c = constDblMat "C" 9 9 1 9

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)
-}

{-o
main :: IO ()
main = do
  bestOp <- search 2 someOp optimizations allCostsOne
  putStrLn $ "Cost = " ++ (show $ fst bestOp) ++ "\n" ++
             (prettyPrint 0 $ fst $ operationToC "testOp" $ snd bestOp)

someOp = [matrixAdd tr13c4 g h,
          scalarMultiply tr13c4 alpha tr13c4,
          matrixMultiply k tr13c4 i]

allCostsOne stmts =
  let (opC, argInfo) = operationToC "testOp" stmts in
  do
    timeResStr <- runTimingCode "mainTiming" opC argInfo
    return $ read $ L.head $ L.lines timeResStr

optimizations :: [[Statement] -> [Statement]]
optimizations =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddM, iConst 1),
   (blockMatrixAddN, iConst 1),
   (blockMatrixAddM, iConst 3),
   (blockMatrixAddN, iConst 11),
   (blockScalarMultiplyM, iConst 1),
   (blockScalarMultiplyN, iConst 3),
   (blockScalarMultiplyN, iConst 4),
   (blockMatrixMultiplyM, iConst 1),
   (blockMatrixMultiplyN, iConst 1),
   (blockMatrixMultiplyP, iConst 1),
   (blockMatrixTransposeM, iConst 1),
   (blockMatrixTransposeM, iConst 6),
   (blockMatrixTransposeN, iConst 3),
   (blockMatrixTransposeN, iConst 1)]

blkUniqueVar :: (IExpr -> IExpr -> Statement -> [Statement]) -> IExpr -> [Statement] -> [Statement]
blkUniqueVar blkFunc blkFactor stmts =
  let v = uniqueVarName stmts in
  expandStatementsBU (blkFunc v blkFactor) stmts

uniqueVarName stmts =
  let vs = uniqueVars stmts in
  case vs of
    [] -> iVar "i"
    (v:rest) ->iVar $ (varName v) ++ "_unique"

uniqueVars stmts = L.nub $ L.concatMap (collectValuesFromStmt loopIVar) stmts

loopIVar stmt =
  case isLoop stmt of
    True -> [iVar $ loopInductionVariable stmt]
    False -> []
-}
main :: IO ()
main = do
  operations <- sequence $ L.map (applyRandomOptimizations blockingOptimizations) testOperations
  avgCyclesPerRun <- sequence $ L.map timeImpl operations
  let flopCostEstimates = L.map flopCost operations
      fakePlot = dblScatterPlotComp "Crude flop estimate" $ L.zip avgCyclesPerRun flopCostEstimates in
    writeReportHtml "CrudeFlopEstimate" $ report "CrudeFlopEstimate" [fakePlot]

timeImpl :: [Statement] -> IO Double
timeImpl op =
  let (cOp, argInfo) = operationToC "testingCostModel" op in
  do
    timeResStr <- runTimingCode "costModelTiming" cOp argInfo
    return $ read $ L.head $ L.lines timeResStr

