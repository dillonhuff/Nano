module SanityCheck(runSanityCheck) where

import Data.List as L

import CBackEnd.CodeGeneration
import CBackEnd.Syntax
import SanityCheckHarness
import System.Settings
import System.Utils

runSanityCheck :: FilePath -> CTopLevelItem String -> CTopLevelItem String -> [ArgumentInfo] -> IO String
runSanityCheck testName scFunc testFunc argInfo =
  let scHarness = sanityCheckHarness (cFuncName scFunc) (cFuncName testFunc) argInfo
      scHeader = cInclude "\"utils.h\""
      codeItems = [scHeader, scFunc, testFunc, scHarness, scMain (dataFileName testName)] in
  do
    compileAndRunC testName codeItems
    readFileShowingContents $ dataFileName testName

compileAndRunC :: FilePath -> [CTopLevelItem String] -> IO ()
compileAndRunC testName codeItems =
  let codeString = L.concat $ L.intersperse "\n" $ L.map (prettyPrint 0) codeItems in
  do
    writeFile (cFileName testName) codeString
    runCommandStrict $ compileString testName
    runCommandStrict $ runString testName  
      
scMain :: FilePath -> CTopLevelItem String
scMain resultFilePath =
  cFuncDecl cInt "main" [] $
            cBlock [(cPtr cFILE, "data_file")]
                   [cExprSt (cAssign (cVar "data_file") (cFuncall "fopen" [cVar ("\"" ++ resultFilePath ++ "\""), cVar "\"w\""])) "",
                    cExprSt (cFuncall "sanity_check" [cVar "data_file"]) "",
                    cExprSt (cFuncall "fclose" [cVar "data_file"]) "",
                    cReturn (cIntLit 0) ""]

