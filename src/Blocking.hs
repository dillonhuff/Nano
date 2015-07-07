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
  block isMatrixAdd (\stmt -> numRows $ operandWritten stmt) indVar blkFactor (Just Row, [Just Row, Just Row]) stmt

blockMatrixAddN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddN indVar blkFactor stmt =
  block isMatrixAdd (\stmt -> numCols $ operandWritten stmt) indVar blkFactor (Just Col, [Just Col, Just Col]) stmt

blockMatrixTransposeM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixTransposeM indVar blkFactor stmt =
  block isMatrixTranspose (\stmt -> numRows $ operandWritten stmt) indVar blkFactor (Just Row, [Just Col]) stmt

blockMatrixTransposeN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixTransposeN indVar blkFactor stmt =
  block isMatrixTranspose (\stmt -> numCols $ operandWritten stmt) indVar blkFactor (Just Col, [Just Row]) stmt

blockScalarMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyM indVar blkFactor stmt =
  block isScalarMultiply (\stmt -> numRows $ operandWritten stmt) indVar blkFactor (Just Row, [Nothing, Just Row]) stmt

blockScalarMultiplyN :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyN indVar blkFactor stmt =
  block isScalarMultiply (\stmt -> numCols $ operandWritten stmt) indVar blkFactor (Just Col, [Nothing, Just Col]) stmt

blockMatrixMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyM indVar blkFactor stmt =
  block isMatrixMultiply (\stmt -> numRows $ operandWritten stmt) indVar blkFactor (Just Row, [Just Row, Nothing, Just Row]) stmt

blockMatrixMultiplyN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyN indVar blkFactor stmt =
  block isMatrixMultiply (\stmt -> numCols $ operandWritten stmt) indVar blkFactor (Just Col, [Nothing, Just Col, Just Col]) stmt

blockMatrixMultiplyP :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyP indVar blkFactor stmt =
  block isMatrixMultiply (\stmt -> numRows $ operandRead 1 stmt) indVar blkFactor (Nothing, [Just Col, Just Row, Nothing]) stmt

block isTargetOp aPartitionedDim indVar blkFactor partDirs stmt =
  case isTargetOp stmt && aPartitionedDim stmt > blkFactor of
    True -> blkStmt aPartitionedDim indVar blkFactor partDirs stmt
    False -> [stmt]

blkStmt aPartitionedDim indVar blkFactor partDirs stmt =
  L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residualOp]
  where
    (mainOp, residualOp) = splitStmt indVar blkFactor partDirs stmt
    mainLoop = blockedLoop indVar (aPartitionedDim stmt) blkFactor [mainOp]

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
