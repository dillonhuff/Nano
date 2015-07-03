module SMulToBroadcast(smulToBroadcast) where

import Control.Monad.State
import Data.List as L

import IndexExpression
import Matrix
import Statement

smulToBroadcast u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToScalarize u) stmts) (uniqueVarPrefix, 0)

tryToScalarize :: Int -> Statement -> State (String, Int) [Statement]
tryToScalarize u stmt =
  case opcode stmt == SMUL && isRegisterizeable u (operandRead 0 stmt) of
    True -> scalarizeSMul stmt
    False -> return [stmt]

isScalarOp :: Int -> Statement -> Bool
isScalarOp u stmt = L.all (isRegisterizeable u) $ allOperands stmt

isRegisterizeable i op =
  case i == 1 of
    True -> isScalar op
    False ->
      case let u = iConst i in (numRows op == u && numCols op == (iConst 1)) || (numCols op == u && numRows op == (iConst 1)) of
        True -> True
        False -> False

scalarizeSMul stmt =
  let c = operandWritten stmt
      alpha = leftOperand stmt
      b = rightOperand stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister r1Name alpha in
      return [broadcast r1 alpha, elemWiseMultiply c r1 b]

duplicateInRegister rName a =
  setName rName $ setRegister a

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i
