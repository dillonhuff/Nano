module Analysis.Matrix(matricesOverlap) where

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
  error "accessedRegionsOverlap"
{-  case accessedRegionsOverlapM ranges s t of
    Just 
  let sRegion = accessedRectangle ranges s
      tRegion = accessedRectangle ranges t in
  rectanglesOverlap sRegion tRegion-}

accessedRegion ranges m =
  error "accessedRegion"

