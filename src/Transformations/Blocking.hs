module Transformations.Blocking(blockMatrixAddM, blockMatrixAddN,
                blockMatrixTransposeM, blockMatrixTransposeN,
                blockScalarMultiplyM, blockScalarMultiplyN,
                blockMatrixMultiplyM, blockMatrixMultiplyN, blockMatrixMultiplyP,
                blockSetZeroM, blockSetZeroN,
                blockSetM, blockSetN,
                blockedLoop, computeResidual, blockingsInDir, operandsPartitionedByBlocking,
                blockingApplies, block,
                blockMAddM, blockMAddN, blockMTransM, blockMTransN, blockSMulM,
                blockSMulN, blockMMulM, blockMMulN, blockMMulP) where

import Data.List as L
import Data.Maybe

import Core.IndexExpression
import Core.Matrix
import Core.Partition
import Core.Statement

blockMatrixAddM :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddM indVar blkFactor stmt =
  block blockMAddM indVar blkFactor stmt

blockMatrixAddN :: IExpr -> IExpr -> Statement -> [Statement]
blockMatrixAddN indVar blkFactor stmt =
  block blockMAddN indVar blkFactor stmt

blockSetM :: IExpr -> IExpr -> Statement -> [Statement]
blockSetM indVar blkFactor stmt =
  block blockMSetM indVar blkFactor stmt

blockSetN :: IExpr -> IExpr -> Statement -> [Statement]
blockSetN indVar blkFactor stmt =
  block blockMSetN indVar blkFactor stmt

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

blockSetZeroM :: IExpr -> IExpr -> Statement -> [Statement]
blockSetZeroM indVar blkFactor stmt =
  block blockZeroM indVar blkFactor stmt

blockSetZeroN :: IExpr -> IExpr -> Statement -> [Statement]
blockSetZeroN indVar blkFactor stmt =
  block blockZeroN indVar blkFactor stmt

blockMAddM =
  blocking isMatrixAdd (\stmt -> numRows $ operandWritten stmt) (Just Row, [Just Row, Just Row])
blockMAddN =
  blocking isMatrixAdd (\stmt -> numCols $ operandWritten stmt) (Just Col, [Just Col, Just Col])
blockMSetM =
  blocking isMatrixSet (\stmt -> numRows $ operandWritten stmt) (Just Row, [Just Row])
blockMSetN =
  blocking isMatrixSet (\stmt -> numCols $ operandWritten stmt) (Just Col, [Just Col])
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
blockZeroM =
  blocking (\stmt -> opcode stmt == ZERO) (\stmt -> numRows $ operandWritten stmt) (Just Row, [])
blockZeroN =
  blocking (\stmt -> opcode stmt == ZERO) (\stmt -> numCols $ operandWritten stmt) (Just Col, [])

blockings =
  [blockMAddM,
   blockMAddN,
   blockMSetM,
   blockMSetN,
   blockMTransM,
   blockMTransN,
   blockSMulM,
   blockSMulN,
   blockMMulM,
   blockMMulN,
   blockMMulP,
   blockZeroM,
   blockZeroM]

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
  case isTargetOp stmt &&
       (((isConst $ aPartitionedDim stmt) && (aPartitionedDim stmt > blkFactor))
        || ((isVar $ aPartitionedDim stmt) && (not $ aPartitionedDim stmt == iConst 1)))  of
    True -> blkStmt aPartitionedDim indVar blkFactor partDirs stmt
    False -> [stmt]

blkStmt aPartitionedDim indVar blkFactor partDirs stmt =
  case isConst $ aPartitionedDim stmt of
    True -> blkConst aPartitionedDim indVar blkFactor partDirs stmt
    False -> case isVar $ aPartitionedDim stmt of
      True -> blkVarDim aPartitionedDim indVar blkFactor partDirs stmt
      False -> error $ "blockStmt: Trying to partition statement " ++ show stmt ++ " by " ++ show blkFactor

blkVarDim aPartitionedDim indVar blkFactor partDirs stmt =
  if blkFactor == iConst 1 then [mainLoop] else [mainLoop, residualLoop]
  where
    resIVar = iVar $ (varName indVar) ++ "_res"
    (mainSt, resSt) = splitStmtGS indVar resIVar blkFactor partDirs stmt
    mainLoop = blockedLoop indVar (aPartitionedDim stmt) blkFactor [mainSt]
    residualLoop = blockedResLoop resIVar (aPartitionedDim stmt) blkFactor [resSt]

splitStmtGS indVar resIndVar blkFactor (splitW, splitR) stmt =
  let (mainW, resW) = (splitMatNGS indVar resIndVar blkFactor splitW) (operandWritten stmt)
      mainRResRPairs = L.zipWith (\dir m -> splitMatNGS indVar resIndVar blkFactor dir m) splitR $ operandsRead stmt
      mainR = L.map fst mainRResRPairs
      resR = L.map snd mainRResRPairs
      mainSt = setOperandWritten mainW $ setOperandsRead mainR stmt
      resSt = setOperandWritten resW $ setOperandsRead resR stmt in
  (mainSt, resSt)

splitMatNGS indVar resIndVar blkFactor maybeDir m =
  case maybeDir of
    Just d -> splitMatGS indVar resIndVar blkFactor d m
    Nothing -> (m, m)

splitMatGS indVar resIndVar blkFactor partDir m =
  case partDir of
    Row -> (rowPart indVar blkFactor m, rowPart resIndVar (iConst 1) m)
    Col -> (colPart indVar blkFactor m, colPart resIndVar (iConst 1) m)

blkConst aPartitionedDim indVar blkFactor partDirs stmt =
  L.filter (\stmt -> not $ anyNullOperands stmt) [mainLoop, residualOp]
  where
    (mainOp, residualOp) = splitStmt indVar blkFactor partDirs stmt
    mainLoop = blockedLoop indVar (aPartitionedDim stmt) blkFactor [mainOp]

blockedLoop indVar dim blkFactor stmts =
  let e = evaluateIExprConstants $ iSub (iMul (iDiv dim blkFactor) blkFactor) (iConst 1) in
  loop (varName indVar) (iConst 0) blkFactor e stmts

blockedResLoop indVar dim blkFactor stmts =
  let s = evaluateIExprConstants $ iMul (iDiv dim blkFactor) blkFactor
      e = evaluateIExprConstants $ iSub dim (iConst 1) in
  loop (varName indVar) s (iConst 1) e stmts
  
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
