module CBackEnd.CodeGeneration.AVX(avxVarDecls, toAVX) where

import Data.List as L

import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Matrix
import Statement

avxVarDecls stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256Reg, bufName info)) regs
    decls = iVarDecls ++ regDecls
  
toAVX stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVX stmt
    _ -> toAVXIntrinsic stmt

toAVXIntrinsic stmt =
  case opcode stmt of
    EADD -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_add_pd" stmt)) ""]
    MSET -> avxSet stmt
    BRDC -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_sd" [matRExpr 0 stmt])) ""]
    EMUL -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_mul_pd" stmt)) ""]
    _ -> error $ "Unsupported opcode " ++ show stmt --[cExprSt (cAssign (regWName stmt) (regFuncall "whoa!" stmt)) ""]

avxSet stmt =
  case isRegister $ operandWritten stmt of
    True -> avxLoad stmt
    False -> avxStore stmt

avxLoad stmt = [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_load_pd" [matRExpr 0 stmt])) ""]

avxStore stmt = [cExprSt (cFuncall "_mm256_store_pd" [matWExpr stmt, matRExpr 0 stmt]) ""]

regWName stmt = regName $ operandWritten stmt
regName op = cVar $ bufferName op
regFuncall n stmt = cFuncall n $ L.map regName $ operandsRead stmt

matWExpr stmt = matToCExpr $ operandWritten stmt
matRExpr n stmt =
  case isRegister $ operandRead n stmt of
    True -> cVar $ bufferName $ operandRead n stmt
    False -> matToCExpr $ operandRead n stmt
