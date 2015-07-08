module PartitionSearch(partitionSearch) where

import Control.Monad.State
import Data.List as L

import Blocking
import IndexExpression
import InterchangeAndFuse
import Matrix
import MMulToSMul
import Partition
import Statement

partitionSearch :: String -> [Statement] -> [Statement]
partitionSearch prefix stmts =
  let res = evalState (propagatingPartitionSearch stmts) (prefix, 0, []) in
  res

propagatingPartitionSearch stmts =
  case isLv1Op stmts of
    True -> return stmts
    False -> do
      res <- pickPartitionAndPropagate stmts
      cRes <- cleanAndFuse res
      propagatingPartitionSearch cRes

cleanAndFuse :: [Statement] -> State (String, Int, [(Matrix, Shape, Int)]) [Statement]
cleanAndFuse stmts = do
  res <- mmulToSMul stmts
  return $ interchangeAndFuse res
  
pickPartitionAndPropagate stmts = do
  addPartition (pickPartition stmts)
  propagatePartition stmts

pickPartition stmts =
  let m = firstPartitionableOperand stmts in
  (m, partDim m, 1)

partDim m =
  case isRowMajor m of
    True -> Row
    False -> Col

firstPartitionableOperand stmts =
  L.head $ L.filter isPartitionable $ L.concatMap (collectFromAllOperands id) stmts

isPartitionable m = isMatrix m

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
  case L.elem m $ allOperands stmt of
    True ->
      let possibleBlockings = blockingsInDir stmt partDir m in
      case possibleBlockings of
        [] -> return [stmt]
        (blk:rest) -> applyPart blk blkFactor stmt
    False -> return [stmt]

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
