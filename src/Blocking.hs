module Blocking(blockMatrixAddM, blockMatrixAddN,
                blockMatrixTransposeM, blockMatrixTransposeN,
                blockScalarMultiplyM, blockScalarMultiplyN,
                blockMatrixMultiplyM, blockMatrixMultiplyN, blockMatrixMultiplyP,
                blockedLoop, computeResidual, blockingsInDir, operandsPartitionedByBlocking,
                blockMAddM, blockMAddN, blockMTransM, blockMTransN, blockSMulM,
                blockSMulN, blockMMulM, blockMMulN, blockMMulP) where

import Data.List as L
import Data.Maybe

import IndexExpression
import Matrix
import Partition
import Statement

blockMatrixAddM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddM indVar blkFactor stmt =
  block blockMAddM indVar blkFactor stmt

blockMatrixAddN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddN indVar blkFactor stmt =
  block blockMAddN indVar blkFactor stmt

blockMatrixTransposeM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixTransposeM indVar blkFactor stmt =
  block blockMTransM indVar blkFactor stmt

blockMatrixTransposeN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixTransposeN indVar blkFactor stmt =
  block blockMTransN indVar blkFactor stmt

blockScalarMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyM indVar blkFactor stmt =
  block blockSMulM indVar blkFactor stmt

blockScalarMultiplyN :: IExpr -> IExpr -> Statement -> [Statement]
blockScalarMultiplyN indVar blkFactor stmt =
  block blockSMulN indVar blkFactor stmt

blockMatrixMultiplyM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyM indVar blkFactor stmt =
  block blockMMulM indVar blkFactor stmt

blockMatrixMultiplyN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyN indVar blkFactor stmt =
  block blockMMulN indVar blkFactor stmt

blockMatrixMultiplyP :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixMultiplyP indVar blkFactor stmt =
  block blockMMulP indVar blkFactor stmt

blockMAddM =
  blocking isMatrixAdd (\stmt -> numRows $ operandWritten stmt) (Just Row, [Just Row, Just Row])
blockMAddN =
  blocking isMatrixAdd (\stmt -> numCols $ operandWritten stmt) (Just Col, [Just Col, Just Col])
blockMTransM =
  blocking isMatrixTranspose (\stmt -> numRows $ operandWritten stmt) (Just Row, [Just Col])
blockMTransN =
  blocking isMatrixTranspose (\stmt -> numCols $ operandWritten stmt) (Just Col, [Just Row])
blockSMulM =
  blocking isScalarMultiply (\stmt -> numRows $ operandWritten stmt) (Just Row, [Nothing, Just Row])
blockSMulN =
  blocking isScalarMultiply (\stmt -> numCols $ operandWritten stmt) (Just Col, [Nothing, Just Col])
blockMMulM =
  blocking isMatrixMultiply (\stmt -> numRows $ operandWritten stmt) (Just Row, [Just Row, Nothing, Just Row])
blockMMulN =
  blocking isMatrixMultiply (\stmt -> numCols $ operandWritten stmt) (Just Col, [Nothing, Just Col, Just Col])
blockMMulP =
  blocking isMatrixMultiply (\stmt -> numRows $ operandRead 1 stmt) (Nothing, [Just Col, Just Row, Nothing])

blockings =
  [blockMAddM,
   blockMAddN,
   blockMTransM,
   blockMTransN,
   blockSMulM,
   blockSMulN,
   blockMMulM,
   blockMMulN,
   blockMMulP]

blockingsInDir stmt dir m =
  case L.elem m $ allOperands stmt of
    True -> L.filter (\blk -> blocksInDir stmt dir m blk) $ L.filter (\blk -> blockingApplies blk stmt) blockings
    False -> []

blocksInDir :: Statement -> Shape -> Matrix -> Blocking -> Bool
blocksInDir stmt dir m blk =
  let partList = partitionByOperand stmt blk in
  L.all (partitionsOperandInDir dir m) partList

partitionByOperand :: Statement -> Blocking -> [(Matrix, Maybe Shape)]
partitionByOperand stmt (Blocking _ _ (wp, rps)) =
  (operandWritten stmt, wp):(L.zip (operandsRead stmt) rps)

partitionsOperandInDir ::Shape -> Matrix -> (Matrix, Maybe Shape) -> Bool
partitionsOperandInDir dir m (n, s) =
  m /= n || (m == n && s == Just dir)

operandsPartitionedByBlocking stmt blking =
  case blockingApplies blking stmt of
    True -> L.nub $ L.map (\(m, p) -> (m, fromJust p)) $ L.filter (\(m, p) -> case p of { Just d -> True; Nothing -> False }) $ partitionByOperand stmt blking
    False -> []

data Blocking
  = Blocking (Statement -> Bool) (Statement -> IExpr) (Maybe Shape, [Maybe Shape])

instance Show Blocking where
  show (Blocking _ _ ps) = show ps

instance Eq Blocking where
  (==) (Blocking _ _ ps1) (Blocking _ _ ps2) = ps1 == ps2

blocking = Blocking

blockingApplies (Blocking isTargetOp _ _) stmt = isTargetOp stmt

block (Blocking isTargetOp aPartitionedDim partDirs) indVar blkFactor stmt =
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
