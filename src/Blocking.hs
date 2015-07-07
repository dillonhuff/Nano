module Blocking(blockMatrixAddM, blockMatrixAddN,
                blockMatrixTransposeM, blockMatrixTransposeN,
                blockScalarMultiplyM, blockScalarMultiplyN,
                blockMatrixMultiplyM, blockMatrixMultiplyN, blockMatrixMultiplyP,
                blockedLoop, computeResidual) where

import Data.List as L

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

blockMAddM indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    c = operandWritten stmt
    (mainAdd, residual) = splitStmt indVar blkFactor (Just Row, [Just Row, Just Row]) stmt
    mainLoop = blockedLoop indVar (numRows c) blkFactor [mainAdd]


blockMAddN indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    c = operandWritten stmt
    (mainAdd, residual) = splitStmt indVar blkFactor (Just Col, [Just Col, Just Col]) stmt
    mainLoop = blockedLoop indVar (numCols c) blkFactor [mainAdd]


blockSMulM indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    c = operandWritten stmt
    (mainSMul, residual) = splitStmt indVar blkFactor (Just Row, [Nothing, Just Row]) stmt
    mainLoop = blockedLoop indVar (numRows c) blkFactor [mainSMul]


blockSMulN indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    c = operandWritten stmt
    (mainSMul, residual) = splitStmt indVar blkFactor (Just Col, [Nothing, Just Col]) stmt
    mainLoop = blockedLoop indVar (numCols c) blkFactor [mainSMul]


blockMMulM indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    c = operandWritten stmt
    (mainMul, residual) = splitStmt indVar blkFactor (Just Row, [Just Row, Nothing, Just Row]) stmt
    mainLoop = blockedLoop indVar (numRows c) blkFactor [mainMul]

blockMMulN indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    c = operandWritten stmt
    (mainMul, residual) = splitStmt indVar blkFactor (Just Col, [Nothing, Just Col, Just Col]) stmt
    mainLoop = blockedLoop indVar (numCols c) blkFactor [mainMul]

blockMMulP indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    b = operandRead 1 stmt
    (mainMul, residual) = splitStmt indVar blkFactor (Nothing, [Just Col, Just Row, Nothing]) stmt
    mainLoop = blockedLoop indVar (numRows b) blkFactor [mainMul]

blockTransM indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    a = operandWritten stmt
    (mainTrans, residual) = splitStmt indVar blkFactor (Just Row, [Just Col]) stmt
    mainLoop = blockedLoop indVar (numRows a) blkFactor [mainTrans]

blockTransN indVar blkFactor stmt = L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residual]
  where
    a = operandWritten stmt
    (mainTrans, residual) = splitStmt indVar blkFactor (Just Col, [Just Row]) stmt
    mainLoop = blockedLoop indVar (numCols a) blkFactor [mainTrans]


blockedLoop indVar dim blkFactor stmts =
  let e = evaluateIExprConstants $ iSub dim blkFactor in
  loop (varName indVar) (iConst 0) blkFactor e stmts

splitStmt indVar blkFactor (splitW, splitR) stmt =
  let (mainW, resW) = (splitMatN indVar blkFactor splitW) (operandWritten stmt)
      mainRResRPairs = L.zipWith (\dir m -> splitMatN indVar blkFactor dir m) splitR $ operandsRead stmt
      mainR = L.map fst mainRResRPairs
      resR = L.map snd mainRResRPairs
      mainSt = setOperandWritten mainW $ setOperandsRead mainR stmt
      resSt = setOperandWritten resW $ setOperandsRead resR stmt in
  (mainSt, resSt)

splitMatN indVar blkFactor maybeDir m =
  case maybeDir of
    Just d -> splitMat indVar blkFactor d m
    Nothing -> (m, m)

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
