module InterchangeAndFuse(interchangeAndFuse) where

import Data.List as L

import Fusion
import Statement
import StatementInterchange

interchangeAndFuse stmts = applyToLoopBodiesBU (tryToInterchangeAndFuse $ iVarRanges stmts) stmts

tryToInterchangeAndFuse iRanges (l1:stmts) =
  case nextLoopAndStmtsBetween stmts of
    Just (l2, stmtsBetween, rest) -> 
      case canFuseIfAdjacent l1 l2 && (L.and $ L.map (\stmt -> canInterchange iRanges stmt l2) stmtsBetween) of
        True -> tryToInterchangeAndFuse iRanges ((fuseLoops l1 l2):(stmtsBetween ++ rest))
        False -> l1 : (tryToInterchangeAndFuse iRanges (stmtsBetween ++ [l2] ++ rest))
    Nothing -> l1:stmts
tryToInterchangeAndFuse iRanges other = other

nextLoopAndStmtsBetween [] = Nothing
nextLoopAndStmtsBetween stmts =
  let stmtsBetween = L.takeWhile (\stmt -> not $ isLoop stmt) stmts in
  case L.length stmtsBetween == L.length stmts of
    True -> Nothing
    False ->
      let loopAndStmtsAfter = L.dropWhile (\stmt -> not $ isLoop stmt) stmts
          nextLoop = L.head loopAndStmtsAfter
          stmtsAfter = L.tail loopAndStmtsAfter in
      Just (nextLoop, stmtsBetween, stmtsAfter)
