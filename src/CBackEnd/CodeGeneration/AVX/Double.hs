module CBackEnd.CodeGeneration.AVX.Double(avxVarDeclsDouble, stmtsToAVXDouble) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.AVX.Common
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

avxVarDeclsDouble stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256dReg, bufName info)) regs
    decls = iVarDecls ++ regDecls ++ tempBufferDecls

stmtsToAVXDouble stmts =
  L.concatMap toAVXDouble stmts

toAVXDouble stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVXDouble stmt
    _ -> firstToMatch avxDoubleInstructions stmt
