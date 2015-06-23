module Blocking(blockMatrixAddM, blockMatrixAddN,
                blockScalarMultiplyM) where

import IndexExpression
import Matrix
import Statement

blockMatrixAddM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddM indVar blockFactor stmt =
  case isMatrixAdd stmt of
    True -> blockMAddM indVar blockFactor stmt
    False -> [stmt]

blockMatrixAddN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddN indVar blkFactor stmt =
  case isMatrixAdd stmt of
    True -> blockMAddN indVar blkFactor stmt
    False -> [stmt]

blockScalarMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyM indVar blkFactor stmt =
  case isScalarMultiply stmt of
    True -> blockSMulM indVar blkFactor stmt
    False -> [stmt]

blockMAddM indVar blkFactor stmt =
  case numRows (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    rs = residualStart blkFactor (numRows c)
    rl = residualLength blkFactor (numRows c)
    e = evaluateIExprConstants $ iSub (numRows c) blkFactor
    mainAdd = applyToOperands (\m -> subMatrix indVar blkFactor (iConst 0) (numCols m) m) stmt
    mainLoop = loop (varName indVar) (iConst 0) blkFactor e [mainAdd]
    residual = applyToOperands (\m -> subMatrix rs rl (iConst 0) (numCols m) m) stmt

blockMAddN indVar blkFactor stmt =
  case numCols (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    rs = residualStart blkFactor (numRows c)
    rl = residualLength blkFactor (numRows c)
    e = evaluateIExprConstants $ iSub (numCols c) blkFactor
    mainAdd = applyToOperands (\m -> subMatrix (iConst 0) (numRows m) indVar blkFactor m) stmt
    mainLoop = loop (varName indVar) (iConst 0) blkFactor e [mainAdd]
    residual = applyToOperands (\m -> subMatrix (iConst 0) (numRows m) rs rl m) stmt

blockSMulM indVar blkFactor stmt =
  case numRows (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    rs = residualStart blkFactor (numRows c)
    rl = residualLength blkFactor (numRows c)
    e = evaluateIExprConstants $ iSub (numRows c) blkFactor
    mainAdd = applyToOperands (\m -> if isMatrix m then subMatrix indVar blkFactor (iConst 0) (numCols m) m else m) stmt
    mainLoop = loop (varName indVar) (iConst 0) blkFactor e [mainAdd]
    residual = applyToOperands (\m -> if isMatrix m then subMatrix rs rl (iConst 0) (numCols m) m else m) stmt

residualStart blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ dimC - (mod dimC blkC)

residualLength blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ mod dimC blkC

