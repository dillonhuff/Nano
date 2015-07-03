module CBackEnd.SanityCheck(runSanityCheck) where

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
      scMain = mainFunc ["sanity_check"] (dataFileName testName)
      codeItems = [avxHeader, scHeader, scFunc, testFunc, scHarness, scMain] in
  do
    compileAndRunC testName codeItems
    readFileShowingContents $ dataFileName testName
