module Transformations.BlockDot(blockDot) where

import Control.Monad.State
import Data.List as L

import Transformations.Blocking
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Utils

blockDot u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryBlkDot (iConst u)) stmts) (uniqueVarPrefix, 0)

tryBlkDot :: IExpr -> Statement -> State (String, Int) [Statement]
tryBlkDot u stmt =
  case opcode stmt == MMUL && isScalar (operandWritten stmt) &&
       ((isFixedSize (operandRead 0 stmt) && isConst u && (constVal u) < (constVal $ numCols $ operandRead 0 stmt)) ||
        isVar (numCols $ operandRead 0 stmt)) of
    True -> blkDot u stmt
    False -> return [stmt]

blkDot u stmt = do
  mainOp <- mainBlkedDot u stmt
  residualOp <- residualBlkedDot u stmt
  return $ L.filter (\st -> not $ anyNullOperands st) (mainOp ++ residualOp)

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
  case isConst $ (numCols $ operandRead 0 stmt) of
    True -> do
      t3N <- freshRegName
      arN <- freshRegName
      let alpha = operandWritten stmt
          x = operandRead 0 stmt
          p = operandRead 1 stmt
          (rs, rl) = computeResidual u (numCols x)
          ar = duplicateInRegister (iConst 1) arN alpha
          xr = colPart rs rl x
          pr = rowPart rs rl p
          t3 = duplicateInRegister u t3N xr in
        return [elemWiseMultiply t3 xr pr, matrixSet ar alpha, accumulate ar ar t3, matrixSet alpha ar]
    False -> do
      i <- freshRegName
      let iV = iVar i
          mainAndRes = blockMatrixMultiplyP iV u stmt
          res = case mainAndRes of { [x] -> []; [x, y] -> [y] } in
        return res

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i
