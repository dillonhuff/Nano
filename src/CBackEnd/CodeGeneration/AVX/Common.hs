module CBackEnd.CodeGeneration.AVX.Common(stmtsToAVX,
                                          avxInstructions,
                                          avxSingleInstructions,
                                          avxDoubleInstructions) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.AVX.CodeGenState
import CBackEnd.CodeGeneration.AVX.CodeSnippets
import CBackEnd.CodeGeneration.AVX.OpcodeTests
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

stmtsToAVX stmts =
  let (body, cgs) = runState (toAVXStmts stmts) $ codeGenState "cgr"
      cgVars = codeGenVars cgs in
  (avxVarDecls stmts ++ cgVars, body)

toAVXStmts :: [Statement] -> State CodeGenState [CStmt String]
toAVXStmts stmts = do
  newStmts <- liftM L.concat $ sequence $ L.map toAVX stmts
  return newStmts

avxVarDecls stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256dReg, bufName info)) regs
    decls = iVarDecls ++ regDecls ++ tempBufferDecls

toAVX :: Statement -> State CodeGenState [CStmt String]
toAVX stmt =
  case opcode stmt of
    LOOP -> loopToCStmtsM toAVX stmt
    _ -> firstToMatch avxInstructions stmt

avxInstructions = avxSingleInstructions ++ avxDoubleInstructions

avxSingleInstructions =
  [(fits_mm256_add 8 single, mm256_add_ps),
   (fits_mm256_mul 8 single, mm256_mul_ps),
   (fits_mm256_fmadd 8 single, mm256_fmadd_ps),
   (fits_mm256_setzero 8 single, mm256_setzero_ps),
   (fits_mm256_broadcast 8 single, mm256_broadcast_ss),
   (fits_mm256_loadu 8 single, mm256_loadu_ps),
   (fits_mm256_storeu 8 single, mm256_storeu_ps),
   (fits_mm256_maskload 8 single, mm256_maskload_ps),
   (fits_mm256_maskstore 8 single, mm256_maskstore_ps),
   (fits_assign, assign)]

mm256_add_ps stmt = return $ ra stmt "_mm256_add_ps"
mm256_mul_ps stmt = return $ ra stmt "_mm256_mul_ps"
mm256_fmadd_ps stmt = return $ ra stmt "_mm256_fmadd_ps"
mm256_setzero_ps stmt = return $ afc (bufferName $ operandWritten stmt) "_mm256_setzero_ps" []
mm256_broadcast_ss stmt = return $ loadInstr stmt "_mm256_broadcast_ss"
mm256_loadu_ps stmt = return $ loadInstr stmt "_mm256_loadu_ps"
mm256_storeu_ps stmt =
  return $ fc "_mm256_storeu_ps" [matWExpr stmt, matRExpr 0 stmt]
mm256_maskload_ps stmt =
  return $ [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_ps" [matRExpr 0 stmt, mask $ vecWidth $ operandRead 0 stmt])) ""]
mm256_maskstore_ps stmt =
  return $ fc "_mm256_maskstore_ps" [matWExpr stmt, mask $ vecWidth $ operandWritten stmt, matRExpr 0 stmt]

avxDoubleInstructions =
  [(fits_mm256_add 4 double, mm256_add_pd),
   (fits_mm256_mul 4 double, mm256_mul_pd),
   (fits_mm256_fmadd 4 double, mm256_fmadd_pd),
   (fits_mm256_setzero 4 double, mm256_setzero_pd),
   (fits_mm256_broadcast 4 double, mm256_broadcast_pd),
   (fits_mm256_loadu 4 double, mm256_loadu_pd), 
   (fits_mm256_storeu 4 double, mm256_storeu_pd),
   (fits_mm256_maskload 4 double, mm256_maskload_pd),
   (fits_mm256_maskstore 4 double, mm256_maskstore_pd), 
   (fits_accum4, accum4),
   (fits_rrbroadcast, rrbroadcast),
   (fits_assign, assign)]

mm256_add_pd stmt = return $ ra stmt "_mm256_add_pd"
mm256_mul_pd stmt = return $ ra stmt "_mm256_mul_pd"
mm256_fmadd_pd stmt = return $ ra stmt "_mm256_fmadd_pd"
mm256_setzero_pd stmt = return $ afc (bufferName $ operandWritten stmt) "_mm256_setzero_pd" []
mm256_broadcast_pd stmt = return $ loadInstr stmt "_mm256_broadcast_sd"
mm256_loadu_pd stmt = return $ loadInstr stmt "_mm256_loadu_pd"
mm256_storeu_pd stmt =
  return $ fc "_mm256_storeu_pd" [matWExpr stmt, matRExpr 0 stmt]
mm256_maskload_pd stmt =
  return $ [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_pd" [matRExpr 0 stmt, maskDbl $ vecWidth $ operandRead 0 stmt])) ""]
mm256_maskstore_pd stmt =
  return $ fc "_mm256_maskstore_pd" [matWExpr stmt, maskDbl $ vecWidth $ operandWritten stmt, matRExpr 0 stmt]
assign stmt =
  return $ [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""]

vecWidth m =
  max (constVal $ numRows m) (constVal $ numCols m)

loadInstr stmt instrName =
  [cExprSt (cAssign (regWName stmt) (cFuncall instrName [matRExpr 0 stmt])) ""]
  
ra stmt instrName =
  [cExprSt (cAssign (regWName stmt) (regFuncall instrName stmt)) ""]
