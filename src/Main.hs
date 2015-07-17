module Main(main) where

import Data.List as L
import System.Random

import Benchmarking
import CBackEnd.CodeGeneration.AVX.Common
import CBackEnd.Syntax
import CBackEnd.Timing
import Core.MatrixOperation
import FrontEnd.Lexer
import FrontEnd.Parser
import Fuzz
import Operations
import OptimizationGroups.Level1
import PartitionSearch
import Reporting.Report
import System.Settings

testFile = "madd.lspc"

main = do
  contents <- readFile testFile
  let parseRes = lexAndParseOperation testFile contents in
    case parseRes of
      Left err -> putStrLn err
      Right op -> do
        dimsAndTimes <- benchmarkOperationGS dimValsList [] lv2Opts avxVarDecls stmtsToAVX op
        let dtRes = L.map (\(dimVals, runTime) -> (snd $ head dimVals, runTime)) dimsAndTimes
            rep = report "Timing_a_series" [intDblLinePlot "square matrix add" "m = n" "avg. cycles per run" [("lv2Opts", dtRes)]] in
          writeReportHtml projectPath rep

lv2Opts = (lvl1Opts 4) ++ [partitionSearch "b_"]

lexAndParseOperation fName str = (lexString fName str) >>= (parseOperation fName)

dimValsList = L.map (\(x, y) -> [("m", x), ("n", y)]) $ L.zip vs vs
  where
    vs = [1, 5..100]

{-
main = do
  writeReportHtml projectPath dummyReport
  putStrLn $ "Done"

dummyReport =
  report "Dummy_Report" [dummyLines]

dummyLines =
  intDblLinePlot "Dummy line chart" "Fake dimension" "Fake runtime" [fakeSeries1, fakeSeries2, fakeSeries3]

fakeSeries1 = ("fake 1", vals 0.05)
fakeSeries2 = ("another fake", vals 0.06)
fakeSeries3 = ("still more fakery", vals 0.08)

trial frac = scanl (*) 1 (map f bits)
  where
    b = 0.1

    f True = (1+frac*(1+b))
    f False = (1-frac)
    bits = randoms $ mkStdGen 0

vals :: Double -> [ (Int, Double) ]
vals frac = [(x, y) | (x,y) <- filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] (trial frac)]
  where
    n = 1001
    m = 0
-}
{-testFile = "madd.lspc"

main = do
  contents <- readFile testFile
  let parseRes = lexAndParseOperation testFile contents in
    case parseRes of
      Left err -> putStrLn err
      Right op -> do
        timeRes <- timeOperationGS dimVals [] lv2Opts avxVarDecls stmtsToAVX op
        putStrLn timeRes

lv2Opts = (lvl1Opts 4) ++ [partitionSearch "b_"]

lexAndParseOperation fName str = (lexString fName str) >>= (parseOperation fName)

dimVals = [("m", 45), ("n", 27)]-}


{-
main :: IO ()
main = do
  operations <- sequence $ L.map (applyRandomOptimizations blockingOptimizations) testOperations
  avgCyclesPerRun <- sequence $ L.map timeImpl operations
  let flopCostEstimates = L.map flopCost operations
      fakePlot = dblScatterPlotComp "Crude flop estimate" "avg cycles per run" "flop cost estimate" $ L.zip avgCyclesPerRun flopCostEstimates in
    writeReportHtml "CrudeFlopEstimate" $ report "CrudeFlopEstimate" [fakePlot]
-}
