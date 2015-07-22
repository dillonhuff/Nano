module CBackEnd.CodeGeneration.AVX.Common(stmtsToAVX,
                                          avxInstructions,
                                          avxSingleInstructions,
                                          avxDoubleInstructions) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.AVX.CodeSnippets
import CBackEnd.CodeGeneration.AVX.OpcodeTests
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

stmtsToAVX stmts =
  let (cgVars, body) = toAVXStmts stmts in
  (avxVarDecls stmts ++ cgVars, body)

toAVXStmts stmts =
  ([], L.concatMap toAVX stmts)

avxVarDecls stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256dReg, bufName info)) regs
    decls = iVarDecls ++ regDecls ++ tempBufferDecls

toAVX stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVX stmt
    _ -> firstToMatch avxInstructions stmt

avxInstructions = avxSingleInstructions ++ avxDoubleInstructions

ra stmt instrName =
  [cExprSt (cAssign (regWName stmt) (regFuncall instrName stmt)) ""]

avxSingleInstructions =
  [(fits_mm256_add 8 single, \stmt -> ra stmt "_mm256_add_ps"),
   (fits_mm256_mul 8 single, \stmt -> ra stmt "_mm256_mul_ps"),
   (fits_mm256_fmadd 8 single, \stmt -> ra stmt "_mm256_fmadd_ps"),
   (fits_mm256_setzero 8 single, \stmt -> afc (bufferName $ operandWritten stmt) "_mm256_setzero_ps" []),
   (fits_mm256_broadcast 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_ss" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_ps" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu 8 single, \stmt -> fc "_mm256_storeu_ps" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_ps" [matRExpr 0 stmt, mask $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore 8 single, \stmt -> fc "_mm256_maskstore_ps" [matWExpr stmt, mask $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]

avxDoubleInstructions =
  [(fits_mm256_add 4 double, \stmt -> ra stmt "_mm256_add_pd"),
   (fits_mm256_mul 4 double, \stmt -> ra stmt "_mm256_mul_pd"),
   (fits_mm256_fmadd 4 double, \stmt -> ra stmt "_mm256_fmadd_pd"),
   (fits_mm256_setzero 4 double, \stmt -> afc (bufferName $ operandWritten stmt) "_mm256_setzero_pd" []),
   (fits_mm256_broadcast 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_sd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_pd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu 4 double, \stmt -> fc "_mm256_storeu_pd" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_pd" [matRExpr 0 stmt, maskDbl $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore 4 double, \stmt -> fc "_mm256_maskstore_pd" [matWExpr stmt, maskDbl $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
   (fits_accum4, \stmt -> accum4 stmt),
   (fits_rrbroadcast, \stmt -> rrbroadcast stmt),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]
