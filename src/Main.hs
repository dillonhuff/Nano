module Main(main) where

import Data.List as L

import Blocking
import CBackEnd.CodeGeneration.Core
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import CBackEnd.Timing
import CBackEnd.TimingHarness
import CompactTemps
import CostModel.Flops
import CostModel.FlopsPlusTempAllocs
import Dummies
import Fusion
import Fuzz
import IndexExpression
import Matrix
import Operations
import RegisterizeTemps
import Reporting.Report
import Registerization
import Search.Exhaustive
import Statement

main :: IO ()
main = do
  bestOp <- search 6 (ddotsmul 16) optimizations (\op -> return $ flopsPlusTempAllocs op)
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
   (blockScalarMultiplyM, iConst 1),
   (blockMatrixAddN, iConst 1),
   (blockScalarMultiplyN, iConst 1),
   (blockMatrixMultiplyM, iConst 1),
   (blockMatrixMultiplyN, iConst 1),
   (blockMatrixMultiplyP, iConst 1),
   (blockMatrixTransposeM, iConst 1),
   (blockMatrixTransposeM, iConst 1),
   (blockMatrixTransposeN, iConst 1)]

someOp = [scalarMultiply tx alpha x,
          matrixAdd y tx y]

timeImpl :: [Statement] -> IO Double
timeImpl op =
  let (cOp, argInfo) = operationToC "testingCostModel" op in
  do
    timeResStr <- runTimingCode "costModelTiming" cOp argInfo
    return $ read $ L.head $ L.lines timeResStr

{-

main :: IO ()
main = do
  operations <- sequence $ L.map (applyRandomOptimizations blockingOptimizations) testOperations
  avgCyclesPerRun <- sequence $ L.map timeImpl operations
  let flopCostEstimates = L.map flopCost operations
      fakePlot = dblScatterPlotComp "Crude flop estimate" "avg cycles per run" "flop cost estimate" $ L.zip avgCyclesPerRun flopCostEstimates in
    writeReportHtml "CrudeFlopEstimate" $ report "CrudeFlopEstimate" [fakePlot]
-}
