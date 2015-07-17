module CBackEnd.CodeGeneration.AVX.Single(avxVarDeclsSingle, stmtsToAVXSingle) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.AVX.Common
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

avxVarDeclsSingle stmts = avxVarDecls stmts

stmtsToAVXSingle stmts =
  L.concatMap toAVXSingle stmts

toAVXSingle stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVXSingle stmt
    _ -> firstToMatch avxInstructions stmt
