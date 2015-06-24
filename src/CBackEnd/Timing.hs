module CBackEnd.Timing(runTimingCode) where

import CBackEnd.Syntax
import CBackEnd.TimingHarness
import CBackEnd.Utils
import System.Settings
import System.Utils

runTimingCode :: FilePath -> CTopLevelItem String -> [BufferInfo] -> IO String
runTimingCode testName funcToTime argInfo =
  let timeHarness = timingHarness (cFuncName funcToTime) argInfo
      timingMain = mainFunc ["time_impl"] (dataFileName testName)
      scHeader = cInclude "\"utils.h\""
      codeItems = [scHeader, funcToTime, timeHarness, timingMain] in
  do
    compileAndRunC testName codeItems
    readFileShowingContents $ dataFileName testName
