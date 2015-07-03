module CBackEnd.CodeGeneration.Core(operationToC,
                                    bufferInfoList) where

import Data.List as L

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Function
import CBackEnd.CodeGeneration.Scalar
import CBackEnd.Syntax
import CBackEnd.Utils
import Matrix
import Statement

operationToC :: ([Statement] -> [(CType, String)]) ->
                (Statement -> [CStmt String]) ->
                String ->
                [Statement] ->
                (CTopLevelItem String, [BufferInfo])
operationToC varDeclFunc codeGenFunc funcName stmts =
  (cFunction, argInfo) 
  where
    localVarDecls = varDeclFunc stmts
    bufInfo = bufferInfoList stmts
    argInfo = L.filter (\info -> bufScope info == arg) bufInfo
    argDecls = L.map (\info -> (bufType info, bufName info)) argInfo
    cFunction = cFuncDecl cVoid funcName argDecls (cBlock localVarDecls $ funcBody codeGenFunc stmts)

funcBody codeGenFunc stmts = body
  where
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufAllocation = L.map initializeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    tempBufFreeing = L.map freeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    body = tempBufAllocation ++ (L.concatMap codeGenFunc stmts) ++ tempBufFreeing
