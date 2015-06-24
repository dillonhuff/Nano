module CBackEnd.SanityCheck(runSanityCheck) where

import Data.List as L

import CBackEnd.CodeGeneration
import CBackEnd.Syntax
import CBackEnd.SanityCheckHarness
import CBackEnd.Utils
import System.Settings
import System.Utils

runSanityCheck :: FilePath -> CTopLevelItem String -> CTopLevelItem String -> [BufferInfo] -> IO String
runSanityCheck testName scFunc testFunc argInfo =
  let scHarness = sanityCheckHarness (cFuncName scFunc) (cFuncName testFunc) argInfo
      scHeader = cInclude "\"utils.h\""
      codeItems = [scHeader, scFunc, testFunc, scHarness, scMain (dataFileName testName)] in
  do
    compileAndRunC testName codeItems
    readFileShowingContents $ dataFileName testName
      
scMain :: FilePath -> CTopLevelItem String
scMain resultFilePath =
  cFuncDecl cInt "main" [] $
            cBlock [(cPtr cFILE, "data_file")]
                   [cExprSt (cAssign (cVar "data_file") (cFuncall "fopen" [cVar ("\"" ++ resultFilePath ++ "\""), cVar "\"w\""])) "",
                    cExprSt (cFuncall "sanity_check" [cVar "data_file"]) "",
                    cExprSt (cFuncall "fclose" [cVar "data_file"]) "",
                    cReturn (cIntLit 0) ""]
