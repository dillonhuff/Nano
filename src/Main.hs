module Main(main) where

import Data.List as L

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import CBackEnd.Timing
import CBackEnd.TimingHarness
import CompactTemps
import CostModel.Flops
import Dummies
import Fusion
import Fuzz
import IndexExpression
import Matrix
import RegisterizeTemps
import Reporting.Report
import Scalarization
import Search.Exhaustive
import Statement

main :: IO ()
main = do
  bestOp <- search 2 someOp optimizations (\op -> return $ flopCost op)
  putStrLn $ "Cost = " ++ (show $ fst bestOp) ++ "\n" ++
             (prettyPrint 0 $ fst $ operationToC "testOp" $ snd bestOp)

optimizations =
  [fuseInnerLoops, compactTemps, registerizeTemps, scalarizeUniqueVar] ++
  Main.blockingOptimizations

scalarizeUniqueVar stmts =
  let uv = uniqueVarName stmts in
  scalarize (varName uv) stmts
  
blockingOptimizations :: [[Statement] -> [Statement]]
blockingOptimizations =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddM, iConst 1),
   (blockMatrixAddN, iConst 1),
   (blockScalarMultiplyM, iConst 1),
   (blockScalarMultiplyN, iConst 1),
   (blockMatrixMultiplyM, iConst 1),
   (blockMatrixMultiplyN, iConst 1),
   (blockMatrixMultiplyP, iConst 1),
   (blockMatrixTransposeM, iConst 1),
   (blockMatrixTransposeM, iConst 1),
   (blockMatrixTransposeN, iConst 1)]

someOp = [matrixAdd tr13c4 g h,
          scalarMultiply tr13c4 alpha tr13c4,
          matrixMultiply k tr13c4 i]

{-
someOp = [matrixAdd tr13c4 g h,
          scalarMultiply tr13c4 alpha tr13c4,
          matrixMultiply k tr13c4 i]

allCostsOne stmts =
  let (opC, argInfo) = operationToC "testOp" stmts in
  do
    timeResStr <- runTimingCode "mainTiming" opC argInfo
    return $ read $ L.head $ L.lines timeResStr



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

{-
timeImpl :: [Statement] -> IO Double
timeImpl op =
  let (cOp, argInfo) = operationToC "testingCostModel" op in
  do
    timeResStr <- runTimingCode "costModelTiming" cOp argInfo
    return $ read $ L.head $ L.lines timeResStr

main :: IO ()
main = do
  operations <- sequence $ L.map (applyRandomOptimizations blockingOptimizations) testOperations
  avgCyclesPerRun <- sequence $ L.map timeImpl operations
  let flopCostEstimates = L.map flopCost operations
      fakePlot = dblScatterPlotComp "Crude flop estimate" "avg cycles per run" "flop cost estimate" $ L.zip avgCyclesPerRun flopCostEstimates in
    writeReportHtml "CrudeFlopEstimate" $ report "CrudeFlopEstimate" [fakePlot]
-}
