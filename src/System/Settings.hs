module System.Settings(compileString,
                      projectPath,
                      testPath,
                      runString,
                      dataFileName,
                      cFileName,
                      executableFileName) where

import Data.List as L

projectPath = "/Users/dillon/Haskell/Nano/"
dataFilePath = projectPath ++ "runData/"
evalPath = projectPath ++ "evalDir/"
testPath = projectPath ++ "libs/"

dataFileName opName = dataFilePath ++ opName ++ ".txt"
cFileName opName = evalPath ++ opName ++ ".c"
executableFileName opName = evalPath ++ opName

compileString opName = "clang -O3 -mavx -march=native -mfma -o " ++ (executableFileName opName) ++ " " ++ (cFileName opName) ++ " " ++ (cFileName "utils")
runString opName = executableFileName opName
