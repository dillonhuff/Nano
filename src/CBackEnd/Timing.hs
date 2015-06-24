module CBackEnd.Timing(runTimingCode) where

import CBackEnd.Syntax
import CBackEnd.TimingHarness
import CBackEnd.Utils
import System.Settings
import System.Utils

runTimingCode :: FilePath -> CTopLevelItem String -> [BufferInfo] -> IO String
runTimingCode testName funcToTime argInfo =
  let timeHarness = timingHarness (cFuncName funcToTime) argInfo
      scHeader = cInclude "\"utils.h\""
      codeItems = [scHeader, funcToTime, timeHarness, timingMain (dataFileName testName)] in
  do
    compileAndRunC testName codeItems
    readFileShowingContents $ dataFileName testName
  
timingMain :: FilePath -> CTopLevelItem String
timingMain resultFilePath =
  cFuncDecl cInt "main" [] $
            cBlock [(cPtr cFILE, "data_file")]
                   [cExprSt (cAssign (cVar "data_file") (cFuncall "fopen" [cVar ("\"" ++ resultFilePath ++ "\""), cVar "\"w\""])) "",
                    cExprSt (cFuncall "time_impl" [cVar "data_file"]) "",
                    cExprSt (cFuncall "fclose" [cVar "data_file"]) "",
                    cReturn (cIntLit 0) ""]
