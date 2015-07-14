module Transformations.SMulToBroadcast(smulToBroadcast) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Utils

smulToBroadcast uniqueVarPrefix stmts =
  evalState (expandStatementsBUM smulToBRDC stmts) (uniqueVarPrefix, 0)

smulToBRDC stmt =
  case opcode stmt == SMUL of
    True ->
      let c = operandWritten stmt
          alpha = operandRead 0 stmt
          b = operandRead 1 stmt in
      do
        r1Name <- freshRegName
        let r1 = duplicateInTemp r1Name b in
          return [broadcast r1 alpha, elemWiseMultiply c r1 b]
    False -> return [stmt]

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i
