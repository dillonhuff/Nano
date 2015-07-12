module CBackEnd.CodeGeneration.Scalar(toScalarC) where

import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

toScalarC stmt =
  case isLoop stmt of
    True -> loopToCStmts toScalarC stmt
    False -> case isMatrixAdd stmt of
      True -> scalarMAddToC stmt
      False -> case isScalarMultiply stmt of
        True -> scalarSMulToC stmt
        False -> case isMatrixMultiply stmt of
          True -> scalarMMulToC stmt
          False -> case isMatrixTranspose stmt || isMatrixSet stmt of
            True -> scalarMSetToC stmt
            False -> error $ "toScalarC: Unsupported statement " ++ show stmt

scalarMSetToC stmt =
  let b = operandRead 0 stmt
      a = operandWritten stmt in
  [cExprSt (cAssign (matrixLocExpr a) (matrixLocExpr b)) ""]

scalarMAddToC stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cAdd (matrixLocExpr a) (matrixLocExpr b))) ""]

scalarMMulToC stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cAdd (cMul (matrixLocExpr a) (matrixLocExpr b)) (matrixLocExpr c))) ""]

scalarSMulToC stmt =
  let c = operandWritten stmt
      alpha = operandRead 0 stmt
      b = operandRead 1 stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cMul (matrixLocExpr alpha) (matrixLocExpr b))) ""]
