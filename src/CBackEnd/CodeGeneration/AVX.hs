module CBackEnd.CodeGeneration.AVX(avxVarDecls, toAVX) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
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
    decls = iVarDecls ++ regDecls ++ tempBufferDecls
  
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
    _ -> error $ "Unsupported opcode " ++ show stmt

avxSet stmt =
  case isRegister $ operandWritten stmt of
    True -> case isRegister $ operandRead 0 stmt of
      True -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""]
      False -> avxLoad stmt
    False -> avxStore stmt

-- This is datatype dependent. Instruction selection needs to be refactored to rely more on pattern matching
avxLoad stmt =
  case isRegisterizeable 4 $ operandRead 0 stmt of
    True -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_load_pd" [matRExpr 0 stmt])) ""]
    False -> case isRegisterizeableBelow 4 $ operandRead 0 stmt of
      True -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_pd" [matRExpr 0 stmt, mask $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]
      False -> error $ "avxLoad: Unsupported stmt " ++ show stmt

avxStore stmt = --[cExprSt (cFuncall "_mm256_store_pd" [matWExpr stmt, matRExpr 0 stmt]) ""]
  case isRegisterizeable 4 $ operandWritten stmt of
    True -> [cExprSt (cFuncall "_mm256_store_pd" [matWExpr stmt, matRExpr 0 stmt]) ""]
    False -> case isRegisterizeableBelow 4 $ operandWritten stmt of
      True -> [cExprSt (cFuncall "_mm256_maskstore_pd" [matWExpr stmt, mask $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]) ""]
      False -> error $ "avxStore: Unsupported stmt " ++ show stmt

regWName stmt = regName $ operandWritten stmt
regName op = cVar $ bufferName op
regFuncall n stmt = cFuncall n $ L.map regName $ operandsRead stmt

matWExpr stmt = matToCExpr $ operandWritten stmt
matRExpr n stmt =
  case isRegister $ operandRead n stmt of
    True -> cVar $ bufferName $ operandRead n stmt
    False -> matToCExpr $ operandRead n stmt

mask n =
  cFuncall "_mm256_set_epi32" $ maskArgs n

-- Also datatype depenent
maskArgs n =
  (L.replicate (2*(4 - n)) (cIntLit 0)) ++ (L.replicate (2*n) (cIntLit (-1)))
