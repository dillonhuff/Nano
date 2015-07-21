module CBackEnd.CodeGeneration.Core(operationToC,
                                    bufferInfoList) where

import Data.List as L

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import CBackEnd.CodeGeneration.Scalar
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.Matrix
import Core.Statement

operationToC :: ([Statement] -> ([(CType, String)], [CStmt String])) ->
                String ->
                [Statement] ->
                (CTopLevelItem String, [BufferInfo])
operationToC codeGenFunc funcName stmts =
  (cFunction, argInfo) 
  where
    (localVarDecls, body) = codeGenFunc stmts
    bufInfo = bufferInfoList stmts
    argInfo = L.filter (\info -> bufScope info == arg) bufInfo
    argDecls = L.map (\info -> (bufType info, bufName info)) argInfo
    cFunction = cFuncDecl cVoid funcName argDecls (cBlock localVarDecls $ funcBody body bufInfo)

funcBody :: [CStmt String] ->
            [BufferInfo] ->
            [CStmt String]
funcBody bodyStmts bufInfo = body
  where
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufAllocation = L.map initializeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    tempBufFreeing = L.map freeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    body = tempBufAllocation ++ bodyStmts ++ tempBufFreeing
