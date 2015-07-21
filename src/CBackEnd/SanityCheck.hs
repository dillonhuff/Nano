module CBackEnd.SanityCheck(runSanityCheck,
                            runSanityCheckGS) where

import Data.List as L

import CBackEnd.CodeGeneration.Core
import CBackEnd.Syntax
import CBackEnd.SanityCheckHarness
import CBackEnd.Utils
import System.Settings
import System.Utils

runSanityCheck :: FilePath -> CTopLevelItem String -> CTopLevelItem String -> [BufferInfo] -> IO String
runSanityCheck testName scFunc testFunc argInfo =
  let scHarness = sanityCheckHarness (cFuncName scFunc) (cFuncName testFunc) argInfo
      scHeader = cInclude "\"utils.h\""
      avxHeader = cInclude "<immintrin.h>"
      avxMacrosHeader = cInclude "\"avx_macros.h\""
      scMain = mainFunc ["sanity_check"] (dataFileName testName)
      codeItems = [avxHeader, scHeader, avxMacrosHeader, scFunc, testFunc, scHarness, scMain] in
  do
    compileAndRunC testName codeItems
    contents <- readFileShowingContents $ dataFileName testName
    deleteDataFile $ dataFileName testName
    return contents

runSanityCheckGS :: FilePath ->
                    [(CType, String)] ->
                    [CStmt String] ->
                    ([BufferInfo] -> [CStmt String]) ->
                    ([BufferInfo] -> [CStmt String]) ->
                    CTopLevelItem String ->
                    CTopLevelItem String ->
                    [BufferInfo] ->
                    IO String
runSanityCheckGS testName decls setupCode scFuncall testFuncall scFunc testFunc argInfo =
  let scHarness = sanityCheckHarnessS decls setupCode scFuncall testFuncall argInfo
      scHeader = cInclude "\"utils.h\""
      avxHeader = cInclude "<immintrin.h>"
      avxMacrosHeader = cInclude "\"avx_macros.h\""
      scMain = mainFunc ["sanity_check"] (dataFileName testName)
      codeItems = [avxHeader, scHeader, avxMacrosHeader, scFunc, testFunc, scHarness, scMain] in
  do
    compileAndRunC testName codeItems
    contents <- readFileShowingContents $ dataFileName testName
    deleteDataFile $ dataFileName testName
    return contents
