module Main(main) where

import Data.List as L

import Benchmarking
import CBackEnd.CodeGeneration.AVX.Double
import CBackEnd.Syntax
import CBackEnd.Timing
import FrontEnd.Lexer
import FrontEnd.Parser
import Fuzz
import Core.MatrixOperation
import Operations
import OptimizationGroups.AVXLevel1
import Core.PartitionSearch

testFile = "testOp.lspc"

main = do
  contents <- readFile testFile
  let parseRes = lexAndParseOperation testFile contents in
    case parseRes of
      Left err -> putStrLn err
      Right op ->
        putStrLn $ timeOperationGS dimVals [] lv2Opts avxVarDeclsDouble toAVXDouble op

lv2Opts = (avxLvl1Opts 4) ++ [partitionSearch "b_"]

lexAndParseOperation fName str = (lexString fName str) >>= (parseOperation fName)

dimVals = [("m", 45), ("n", 27)]


{-
main :: IO ()
main = do
  operations <- sequence $ L.map (applyRandomOptimizations blockingOptimizations) testOperations
  avgCyclesPerRun <- sequence $ L.map timeImpl operations
  let flopCostEstimates = L.map flopCost operations
      fakePlot = dblScatterPlotComp "Crude flop estimate" "avg cycles per run" "flop cost estimate" $ L.zip avgCyclesPerRun flopCostEstimates in
    writeReportHtml "CrudeFlopEstimate" $ report "CrudeFlopEstimate" [fakePlot]
-}
