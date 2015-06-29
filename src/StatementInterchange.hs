module StatementInterchange(interchangeStmts) where

import Data.List as L
import Data.Map as M

import Analysis.IndexExpression
import Analysis.Matrix
import IndexExpression
import Statement

interchangeStmts :: [Statement] -> [Statement]
interchangeStmts stmts =
  let varRanges = iVarRanges stmts in
  applyToLoopBodiesBU (tryToInterchangeStmtList varRanges) stmts

tryToInterchangeStmtList _ [] = []
tryToInterchangeStmtList varRanges [stmt] = [stmt]
tryToInterchangeStmtList varRanges (l:r:rest) =
  case canInterchange varRanges l r of
    True -> r:l:(tryToInterchangeStmtList varRanges rest)
    False -> l:r:(tryToInterchangeStmtList varRanges rest)

canInterchange :: Map IExpr (IExpr, IExpr) -> Statement -> Statement -> Bool
canInterchange iRanges l r =
  let lMatrices = allMatrices l
      rMatrices = allMatrices r in
  L.and $ L.map (\(s, t) -> not $ matricesOverlap iRanges s t) [(s, t) | s <- lMatrices, t <- rMatrices]

iVarRanges stmts = error "varRanges"

allMatrices stmt =
  collectFromAllOperands id stmt
