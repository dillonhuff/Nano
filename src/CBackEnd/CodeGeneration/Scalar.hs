module CBackEnd.CodeGeneration.Scalar(toScalarC) where

import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
import Matrix
import Statement

toScalarC stmt =
  case isMatrixAdd stmt of
    True -> scalarMAddToC stmt
    False -> case isScalarMultiply stmt of
      True -> scalarSMulToC stmt
      False -> case isMatrixMultiply stmt of
        True -> scalarMMulToC stmt
        False -> case isMatrixTranspose stmt || isMatrixSet stmt of
          True -> scalarMSetToC stmt
          False -> error $ "toScalarC: Unsupported statement " ++ show stmt

scalarMSetToC stmt =
  let b = rightOperand stmt
      a = operandWritten stmt in
  [cExprSt (cAssign (matrixLocExpr a) (matrixLocExpr b)) ""]

scalarMAddToC stmt =
  let c = operandWritten stmt
      a = leftOperand stmt
      b = rightOperand stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cAdd (matrixLocExpr a) (matrixLocExpr b))) ""]

scalarMMulToC stmt =
  let c = operandWritten stmt
      a = leftOperand stmt
      b = rightOperand stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cAdd (cMul (matrixLocExpr a) (matrixLocExpr b)) (matrixLocExpr c))) ""]

scalarSMulToC stmt =
  let c = operandWritten stmt
      alpha = leftOperand stmt
      b = rightOperand stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cMul (matrixLocExpr alpha) (matrixLocExpr b))) ""]
