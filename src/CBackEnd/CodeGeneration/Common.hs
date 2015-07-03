module CBackEnd.CodeGeneration.Common(loopToCStmts,
                                      scalarVarDecls, inductionVariableDecls,
                                      matToCExpr,
                                      bufferInfoList) where

import Data.List as L

import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
import Matrix
import Statement

loopToCStmts toCStmts l =
  [cFor s e i b ""]
  where
    v = iExprToCExpr $ iVar $ loopInductionVariable l
    s = cAssign v (iExprToCExpr $ loopStart l)
    e = cLEQ v (iExprToCExpr $ loopEnd l)
    i = cAssign v (cAdd v (iExprToCExpr $ loopInc l))
    b = cBlock [] $ L.concatMap toCStmts $ loopBody l

scalarVarDecls :: [Statement] -> [(CType, String)]
scalarVarDecls stmts = localVarDecls
  where
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls tempBufInfo
    iVarDecls = inductionVariableDecls stmts
    localVarDecls = iVarDecls ++ tempBufferDecls

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

matToCExpr m =
  case isRegister m of
    True -> cAddr (cVar $ bufferName m)
    False -> cAdd (cVar $ bufferName m) (iExprToCExpr $ evaluateIExprConstants $ locationExpr m)
