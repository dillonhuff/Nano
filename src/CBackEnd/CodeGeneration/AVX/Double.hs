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

avxVarDeclsDouble stmts = avxVarDecls stmts

stmtsToAVXDouble stmts =
  L.concatMap toAVXDouble stmts

toAVXDouble stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVXDouble stmt
    _ -> firstToMatch avxInstructions stmt
