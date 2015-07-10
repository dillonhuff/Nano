module Analysis.Matrix(allMatIVars,
                       matricesOverlap,
                       isRegisterizeable,
                       isRegisterizeableBelow) where

import Data.List as L
import Data.Map as M

import Analysis.IndexExpression
import IndexExpression
import Matrix

matricesOverlap :: Map IExpr (IExpr, IExpr, IExpr) -> Matrix -> Matrix -> Bool
matricesOverlap ranges s t =
  case bufferName s == bufferName t of
    True -> accessedRegionsOverlap ranges s t
    False -> False

accessedRegionsOverlap :: Map IExpr (IExpr, IExpr, IExpr) -> Matrix -> Matrix -> Bool
accessedRegionsOverlap ranges s t =
  case accessOverlap ranges s t of
    Just b -> b
    Nothing -> True

accessOverlap iRanges s t = do
  sR <- accessedRectangle iRanges s
  tR <- accessedRectangle iRanges t
  error $ "sR = " ++ show sR ++ "\n" ++ "tR = " ++ show tR
  --return $ rectanglesOverlap sR tR

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
