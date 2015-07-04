module Analysis.Matrix(matricesOverlap,
                       isRegisterizeable,
                       isRegisterizeableBelow) where

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
    Nothing -> False

accessOverlap iRanges s t = do
  sR <- accessedRectangle iRanges s
  tR <- accessedRectangle iRanges t
  return $ rectanglesOverlap sR tR

isRegisterizeable i op =
  case i == 1 of
    True -> isScalar op
    False ->
      case let u = iConst i in (numRows op == u && numCols op == (iConst 1)) || (numCols op == u && numRows op == (iConst 1)) of
        True -> True
        False -> False

isRegisterizeableBelow i op =
  case i == 1 of
    True -> isScalar op
    False ->
      case let u = iConst i in (numRows op < u && numCols op == (iConst 1)) || (numCols op == u && numRows op < (iConst 1)) of
        True -> True
        False -> False
