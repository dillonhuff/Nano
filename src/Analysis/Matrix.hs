module Analysis.Matrix(allMatIVars,
                       matricesOverlap,
                       isRegisterizeable,
                       isRegisterizeableBelow) where

import Data.List as L
import Data.Map as M

import Analysis.IndexExpression
import IndexExpression
import Matrix
import Partition

matricesOverlap :: Map IExpr (IExpr, IExpr, IExpr) -> Matrix -> Matrix -> Bool
matricesOverlap ranges s t =
  case bufferName s == bufferName t of
    True -> accessedRegionsOverlap ranges s t
    False -> False

accessedRegionsOverlap :: Map IExpr (IExpr, IExpr, IExpr) -> Matrix -> Matrix -> Bool
accessedRegionsOverlap ranges s t =
  case accessOverlap ranges s t of
    Just b -> b
    Nothing -> not $ anyDisjointPartitions ranges s t

accessOverlap iRanges s t = do
  sR <- accessedRectangle iRanges s
  tR <- accessedRectangle iRanges t
  return $ rectanglesOverlap sR tR

isRegisterizeable u op =
  case isFixedSize op of
    True ->
      case u == 1 of
        True -> isScalar op
        False ->
          case ((constVal $ numRows op) == u && (constVal $ numCols op) == 1)  ||
               ((constVal $ numCols op) == u && (constVal $ numRows op) == 1) of
            True -> True
            False -> False
    False -> False

isRegisterizeableBelow u op =
  case isFixedSize op of
    True ->
      case u == 1 of
        True -> isScalar op
        False ->
          case ((constVal $ numRows op) < u && (constVal $ numCols op) == 1)  ||
               ((constVal $ numCols op) < u && (constVal $ numRows op) == 1) of
            True -> True
            False -> False
    False -> False

allMatIVars m =
  let b = underlyingMatrix m in
  L.filter isVar $ L.concatMap allIExprOperands [numRows b, numCols b, rowStride b, colStride b]

anyDisjointPartitions ranges s t =
  let sParts = partitionList s
      tParts = partitionList t in
  L.and [areDisjoint ranges sp tp | sp <- sParts, tp <- tParts]

areDisjoint ranges p1 p2 =
  case partShape p1 == partShape p2 of
    True -> endsBefore ranges p1 p2 || endsBefore ranges p2 p1
    False -> False

endsBefore ranges p1 p2 =
  let (_, _, e) = lookupF (partBase p1) ranges
      (s, _, _) = lookupF (partBase p2) ranges in
  (evaluateIExprConstants $ iSub s (iConst 1)) == e

lookupF k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> error $ "lookupF: Could not find value"
