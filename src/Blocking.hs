module Blocking(blockMatrixAddM, blockMatrixAddN,
                blockMatrixTransposeM, blockMatrixTransposeN,
                blockScalarMultiplyM, blockScalarMultiplyN,
                blockMatrixMultiplyM, blockMatrixMultiplyN, blockMatrixMultiplyP,
                blockedLoop, computeResidual) where

import IndexExpression
import Matrix
import Partition
import Statement

blockMatrixAddM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddM indVar blkFactor stmt =
  case isMatrixAdd stmt && numRows (operandWritten stmt) > blkFactor of
    True -> blockMAddM indVar blkFactor stmt
    False -> [stmt]

blockMatrixAddN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddN indVar blkFactor stmt =
  case isMatrixAdd stmt && numCols (operandWritten stmt) > blkFactor of
    True -> blockMAddN indVar blkFactor stmt
    False -> [stmt]

blockMatrixTransposeM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixTransposeM indVar blkFactor stmt =
  case isMatrixTranspose stmt && numRows (operandWritten stmt) > blkFactor of
    True -> blockTransM indVar blkFactor stmt
    False -> [stmt]

blockMatrixTransposeN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixTransposeN indVar blkFactor stmt =
  case isMatrixTranspose stmt && numCols (operandWritten stmt) > blkFactor of
    True -> blockTransN indVar blkFactor stmt
    False -> [stmt]

blockScalarMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyM indVar blkFactor stmt =
  case isScalarMultiply stmt && numRows (operandWritten stmt) > blkFactor of
    True -> blockSMulM indVar blkFactor stmt
    False -> [stmt]

blockScalarMultiplyN :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyN indVar blkFactor stmt =
  case isScalarMultiply stmt && numCols (operandWritten stmt) > blkFactor of
    True -> blockSMulN indVar blkFactor stmt
    False -> [stmt]

blockMatrixMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyM indVar blkFactor stmt =
  case isMatrixMultiply stmt && numRows (operandWritten stmt) > blkFactor of
    True -> blockMMulM indVar blkFactor stmt
    False -> [stmt]

blockMatrixMultiplyN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyN indVar blkFactor stmt =
  case isMatrixMultiply stmt && numCols (operandWritten stmt) > blkFactor of
    True -> blockMMulN indVar blkFactor stmt
    False -> [stmt]

blockMatrixMultiplyP :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyP indVar blkFactor stmt =
  case isMatrixMultiply stmt && numCols (operandRead 0 stmt) > blkFactor of
    True -> blockMMulP indVar blkFactor stmt
    False -> [stmt]

blockMAddM indVar blkFactor stmt =
  case numRows (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    (rs, rl) = computeResidual blkFactor (numRows c)
    mainAdd = applyToOperands (\m -> rowPart indVar blkFactor m) stmt
    mainLoop = blockedLoop indVar (numRows c) blkFactor [mainAdd]
    residual = applyToOperands (\m -> rowPart rs rl m) stmt

blockMAddN indVar blkFactor stmt =
  case numCols (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    (rs, rl) = computeResidual blkFactor (numCols c)
    mainAdd = applyToOperands (\m -> colPart indVar blkFactor m) stmt
    mainLoop = blockedLoop indVar (numCols c) blkFactor [mainAdd]
    residual = applyToOperands (\m -> colPart rs rl m) stmt

blockSMulM indVar blkFactor stmt =
  case numRows (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    (rs, rl) = computeResidual blkFactor (numRows c)
    mainSMul = applyToOperands (\m -> if not (isScalar m) then rowPart indVar blkFactor m else m) stmt
    mainLoop = blockedLoop indVar (numRows c) blkFactor [mainSMul]
    residual = applyToOperands (\m -> if not (isScalar m) then rowPart rs rl m else m) stmt

blockSMulN indVar blkFactor stmt =
  case numCols (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    (rs, rl) = computeResidual blkFactor (numCols c)
    mainSMul = applyToOperands (\m -> if not (isScalar m) then colPart indVar blkFactor m else m) stmt
    mainLoop = blockedLoop indVar (numCols c) blkFactor [mainSMul]
    residual = applyToOperands (\m -> if not (isScalar m) then colPart rs rl m else m) stmt

blockMMulM indVar blkFactor stmt =
  case numRows (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    a = operandRead 0 stmt
    b = operandRead 1 stmt
    (mainC, resC) = splitMat indVar blkFactor Row c
    (mainA, resA) = splitMat indVar blkFactor Row a
    mainMul = matrixMultiply mainC mainA b
    (rs, rl) = computeResidual blkFactor (numRows c)
    mainLoop = blockedLoop indVar (numRows c) blkFactor [mainMul]
    residual = matrixMultiply resC resA b

blockMMulN indVar blkFactor stmt =
  case numCols (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    a = operandRead 0 stmt
    b = operandRead 1 stmt
    (mainB, resB) = splitMat indVar blkFactor Col b
    (mainC, resC) = splitMat indVar blkFactor Col c
    mainMul = matrixMultiply mainC a mainB
    (rs, rl) = computeResidual blkFactor (numCols c)
    mainLoop = blockedLoop indVar (numCols c) blkFactor [mainMul]
    residual = matrixMultiply resC a resB

blockMMulP indVar blkFactor stmt =
  case numCols (operandRead 0 residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    a = operandRead 0 stmt
    b = operandRead 1 stmt
    (mainA, resA) = splitMat indVar blkFactor Col a
    (mainB, resB) = splitMat indVar blkFactor Row b
    mainMul = matrixMultiply c mainA mainB
    mainLoop = blockedLoop indVar (numRows b) blkFactor [mainMul]
    residual = matrixMultiply c resA resB

blockTransM indVar blkFactor stmt =
  case numRows (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    a = operandWritten stmt
    b = operandRead 0 stmt
    (mainA, resA) = splitMat indVar blkFactor Row a
    (mainB, resB) = splitMat indVar blkFactor Col b
    (rs, rl) = computeResidual blkFactor (numRows a)
    mainTrans = matrixTranspose mainA mainB
    mainLoop = blockedLoop indVar (numRows a) blkFactor [mainTrans]
    residual = matrixTranspose resA resB

blockTransN indVar blkFactor stmt =
  case numCols (operandWritten residual) == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    a = operandWritten stmt
    b = operandRead 0 stmt
    (mainA, resA) = splitMat indVar blkFactor Col a --colPart indVar blkFactor a
    (mainB, resB) = splitMat indVar blkFactor Row b
    (rs, rl) = computeResidual blkFactor (numCols a)
    mainTrans = matrixTranspose mainA mainB
    mainLoop = blockedLoop indVar (numCols a) blkFactor [mainTrans]
    residual = matrixTranspose resA resB

blockedLoop indVar dim blkFactor stmts =
  let e = evaluateIExprConstants $ iSub dim blkFactor in
  loop (varName indVar) (iConst 0) blkFactor e stmts

splitMat indVar blkFactor partDir m =
  case partDir of
    Row -> let (rs, rl) = computeResidual blkFactor (numRows m) in (rowPart indVar blkFactor m, rowPart rs rl m)
    Col -> let (rs, rl) = computeResidual blkFactor (numCols m) in (colPart indVar blkFactor m, colPart rs rl m)

computeResidual blkFactor dimLength =
  (residualStart blkFactor dimLength, residualLength blkFactor dimLength)
  
residualStart blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ dimC - (mod dimC blkC)

residualLength blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ mod dimC blkC
