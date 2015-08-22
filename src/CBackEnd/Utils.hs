module CBackEnd.Utils(initializeBuffer, freeBuffer,
                      bufSizeExpr,
                      bufDecls,
                      setArgToRandValuesCode,
                      compileAndRunC, mainFunc, deleteFile,
                      matrixLocExpr) where

import Data.List as L

import CBackEnd.Syntax
import Core.IndexExpression
import Core.Matrix
import System.Settings
import System.Utils

initializeBuffer bufInfo = cExprSt (cAssign (cVar $ bufName bufInfo) (cFuncall "malloc" [bufSizeExpr bufInfo])) ""

freeBuffer bufInfo = cExprSt (cFuncall "free" [cVar $ bufName bufInfo]) ""

bufSizeExpr bufInfo = cMul (cSizeOf (getReferencedType $ bufType bufInfo)) (bufSize bufInfo)

bufDecls argInfo = L.map (\info -> bufDecl info) argInfo

bufDecl bufInfo = (bufType bufInfo, bufName bufInfo)

setArgToRandValuesCode :: BufferInfo -> CStmt String
setArgToRandValuesCode argInfo =
  let name = bufName argInfo
      tp = bufType argInfo
      sz = bufSize argInfo in
  case getReferencedType tp == cDouble of
    True -> cExprSt (cFuncall "rand_doubles" [sz, cVar name]) ""
    False -> case getReferencedType tp == cFloat of
      True -> cExprSt (cFuncall "rand_floats" [sz, cVar name]) ""
      False -> error $ "Unrecognized type in setArgToRandValuesCode " ++ show tp

compileAndRunC :: FilePath -> [CTopLevelItem String] -> IO ()
compileAndRunC testName codeItems =
  let codeString = L.concat $ L.intersperse "\n" $ L.map (prettyPrint 0) codeItems in
  do
    writeFile (cFileName testName) codeString
    runCommandStrict $ compileString testName
    runCommandStrict $ runString testName

deleteFile :: FilePath -> IO ()
deleteFile dataFile = do
  runCommandStrict $ deleteFileString dataFile

mainFunc :: [String] -> FilePath -> CTopLevelItem String
mainFunc harnessFuncs resultFilePath =
  cFuncDecl cInt "main" [] $ cBlock [(cPtr cFILE, "data_file")] body
  where
    harnessCalls = L.map (\n -> cExprSt (cFuncall n [cVar "data_file"]) "") harnessFuncs
    body = [cExprSt (cAssign (cVar "data_file") (cFuncall "fopen" [cVar ("\"" ++ resultFilePath ++ "\""), cVar "\"w\""])) ""] ++
           harnessCalls ++
           [cExprSt (cFuncall "fclose" [cVar "data_file"]) "", cReturn (cIntLit 0) ""]

matrixLocExpr m =
  case isRegister m of
    True -> cVar $ bufferName m
    False -> cArrAcc (cVar $ bufferName m) (iExprToCExpr $ locationExpr m)

