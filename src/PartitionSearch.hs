module PartitionSearch(partitionSearch) where

import Control.Monad.State
import Data.List as L

import Blocking
import IndexExpression
import InterchangeAndFuse
import Matrix
import Partition
import Statement

partitionSearch :: String -> [Statement] -> [Statement]
partitionSearch prefix stmts =
  evalState (propagatingPartitionSearch stmts) (prefix, 0, [])

propagatingPartitionSearch stmts =
  case isLv1Op stmts of
    True -> return stmts
    False -> pickPartitionAndPropagate stmts >>= propagatingPartitionSearch 

cleanAndFuse stmts =
  return $ interchangeAndFuse stmts
  
pickPartitionAndPropagate stmts = do
  addPartition (pickPartition stmts)
  propagatePartition stmts

pickPartition stmts =
  (firstOperand stmts, Row, 1)

firstOperand stmts =
  L.head $ L.concatMap (collectFromAllOperands id) stmts

propagatePartition stmts = do
  (p, i, nextParts) <- get
  case nextParts of
    [] -> return stmts
    ((m, partDir, blkFactor):rest) -> do
      popPartition      
      newStmts <- applyPartitionAndCollectInducedPartitions m partDir blkFactor stmts
      propagatePartition newStmts

applyPartitionAndCollectInducedPartitions m partDir blkFactor stmts =
  expandStatementsBUM (applyPartIfPossible m partDir blkFactor) stmts

applyPartIfPossible m partDir blkFactor stmt =
  let possibleBlockings = blockingsInDir stmt partDir m in
  case possibleBlockings of
    [] -> return [stmt]
    (blk:rest) -> applyPart blk blkFactor stmt

applyPart blk blkFactor stmt =
  case blockingApplies blk stmt of
    True -> applyPartWithFactor blk blkFactor stmt
    False -> return [stmt]

applyPartWithFactor blk blkFactor stmt = do
  iN <- freshRegName
  let blockedStmts = block blk (iVar iN) (iConst blkFactor) stmt
      newParts = operandsPartitionedByBlocking stmt blk
      newPartsWithBlkFactors = L.map (\(m, p) -> (m, p, blkFactor)) newParts in
    do
      pushPartitions newPartsWithBlkFactors
      return blockedStmts

isLv1Op stmts =
  L.and $ L.concatMap (collectFromStmt isLvl1Stmt) stmts

isLvl1Stmt stmt =
  isLoop stmt || (L.all (\m -> isVector m || isScalar m) $ allOperands stmt)

freshRegName :: State (String, Int, [(Matrix, Shape, Int)]) String
freshRegName = do
  (prefix, i, pts) <- get
  put $ (prefix, i + 1, pts)
  return $ prefix ++ show i

addPartition :: (Matrix, Shape, Int) -> State (String, Int, [(Matrix, Shape, Int)]) ()
addPartition newPart = do
  (prefix, i, pts) <- get
  put $ (prefix, i, newPart:pts)

popPartition :: State (String, Int, [(Matrix, Shape, Int)]) ()
popPartition = do
  (prefix, i, pts) <- get
  case pts of
    [] -> put (prefix, i, [])
    (pt:rest) -> put (prefix, i, rest)

pushPartitions :: [(Matrix, Shape, Int)] -> State (String, Int, [(Matrix, Shape, Int)]) ()
pushPartitions newParts = do
  (prefix, i, pts) <- get
  put $ (prefix, i, newParts ++ pts)
