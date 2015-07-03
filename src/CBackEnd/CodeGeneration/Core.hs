module CBackEnd.CodeGeneration.Core(operationToC,
                                    bufferInfoList,
                                    inductionVariableDecls) where

import Data.List as L

import CBackEnd.CodeGeneration.Function
import CBackEnd.CodeGeneration.Scalar
import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
import Matrix
import Statement

operationToC :: (Statement -> [CStmt String]) -> String -> [Statement] -> (CTopLevelItem String, [BufferInfo])
operationToC codeGenFunc funcName stmts =
  (cFunction, argInfo) 
  where    
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls tempBufInfo
    tempBufAllocation = L.map initializeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    tempBufFreeing = L.map freeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    body = tempBufAllocation ++ (L.concatMap codeGenFunc stmts) ++ tempBufFreeing
    iVarDecls = inductionVariableDecls stmts
    localVarDecls = iVarDecls ++ tempBufferDecls
    argInfo = L.filter (\info -> bufScope info == arg) bufInfo
    argDecls = L.map (\info -> (bufType info, bufName info)) argInfo
    cFunction = cFuncDecl cVoid funcName argDecls (cBlock localVarDecls body)

toCType :: Type -> CType
toCType t =
  case isDouble t of
    True -> cDouble
    False -> cFloat

inductionVariableDecls :: [Statement] -> [(CType, String)]
inductionVariableDecls stmts =
  let iNames = L.nub $ L.concatMap (collectValuesFromStmt (\st -> if isLoop st then [loopInductionVariable st] else [])) stmts in
  L.zip (L.replicate (length iNames) cInt) iNames

bufferInfoList :: [Statement] -> [BufferInfo]
bufferInfoList stmts =
  let allMats = L.nub $ L.concatMap (collectValuesFromStmt $ collectFromAllOperands matrixBufferInfo) stmts in
  L.sortBy (\l r -> compare (bufName l) (bufName r)) allMats

matrixBufferInfo :: Matrix -> BufferInfo
matrixBufferInfo m =
  case isRegister m of
    True -> bufferInfo (bufferName m) (toCType $ dataType m) (iExprToCExpr $ sizeExpr m) (bufferScope m)
    False -> bufferInfo (bufferName m) (cPtr $ toCType $ dataType m) (iExprToCExpr $ sizeExpr m) (bufferScope m)
