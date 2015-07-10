module Main(main) where

import Data.List as L

import CBackEnd.CodeGeneration.AVX.Double
import CBackEnd.Syntax
import CBackEnd.Timing
import Fuzz
import Operations
import OptimizationGroups.AVXLevel1
import CBackEnd.Timing

main =
  runTimingCodeForExternalFunction "external_func_test" simpleAddCall simpleAddVars addSetup addTearDown (cInclude "\"utils.h\"")

simpleAddCall = [cExprSt (cFuncall "simple_add" [cVar "m", cVar "n",
                                                cVar "a", cVar "ars", cVar "acs",
                                                cVar "b", cVar "brs", cVar "bcs",
                                                cVar "c", cVar "crs", cVar "ccs"]) ""]
addSetup = []

simpleAddVars = []

addTearDown = []

{-main :: IO ()
main = assertOptimizationsCorrect avxVarDecls toAVX avxLvl1Opts (daxpadd 16)-}

{-
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
-}
{-

main :: IO ()
main = do
  operations <- sequence $ L.map (applyRandomOptimizations blockingOptimizations) testOperations
  avgCyclesPerRun <- sequence $ L.map timeImpl operations
  let flopCostEstimates = L.map flopCost operations
      fakePlot = dblScatterPlotComp "Crude flop estimate" "avg cycles per run" "flop cost estimate" $ L.zip avgCyclesPerRun flopCostEstimates in
    writeReportHtml "CrudeFlopEstimate" $ report "CrudeFlopEstimate" [fakePlot]
-}
