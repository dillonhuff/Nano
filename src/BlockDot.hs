module BlockDot(blockDot) where

import Control.Monad.State

import Blocking
import IndexExpression
import Matrix
import Statement
import Utils

blockDot u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryBlkDot (iConst u)) stmts) (uniqueVarPrefix, 0)

tryBlkDot :: IExpr -> Statement -> State (String, Int) [Statement]
tryBlkDot u stmt =
  case opcode stmt == MMUL && isScalar (operandWritten stmt) of
    True -> blkDot u stmt
    False -> return [stmt]

blkDot u stmt = do
  mainOp <- mainBlkedDot u stmt
  residualOp <- residualBlkedDot u stmt
  case mod (constVal $ numCols $ operandRead 0 stmt) (constVal u) == 0 of
    True -> return mainOp
    False -> return $ mainOp ++ residualOp

mainBlkedDot u stmt =
  let alpha = operandWritten stmt
      x = operandRead 0 stmt
      p = operandRead 1 stmt in
  do
    iN <- freshRegName
    t1N <- freshRegName
    t2N <- freshRegName
    aN <- freshRegName
    let i = iVar iN
        xi = colPart i u x
        pi = rowPart i u p
        t1 = duplicateInRegister u t1N xi
        t2 = duplicateInRegister u t2N t1
        ar = duplicateInRegister (iConst 1) aN alpha
        body = [elemWiseMultiply t1 xi pi, matrixAdd t2 t1 t2] in
      return $ (setZero t2):(blockedLoop i (numCols x) u body):(matrixSet ar alpha):(accumulate ar ar t2):(matrixSet alpha ar):[]

residualBlkedDot u stmt =
  error "residualBlkedDot"

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i
