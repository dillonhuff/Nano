module Transformations.InterchangeAndFuse(interchangeAndFuse) where

import Data.List as L

import Analysis.Loop
import Transformations.Fusion
import Core.Statement
import Transformations.StatementInterchange

interchangeAndFuse stmts = applyToLoopBodiesBU (tryToInterchangeAndFuse $ iVarRanges stmts) stmts

tryToInterchangeAndFuse iRanges (l1:stmts) =
  case isLoop l1 of
    True -> 
      case nextLoopWithSameIterSpaceAndStmtsBetween l1 stmts of
        Just (l2, stmtsBetween, rest) -> 
          case canFuseIfAdjacent l1 l2 && (L.and $ L.map (\stmt -> canInterchange iRanges stmt l2) stmtsBetween) of
            True -> tryToInterchangeAndFuse iRanges ((fuseLoops l1 l2):(stmtsBetween ++ rest))
            False -> case canFuseIfAdjacent l1 l2 && (L.and $ L.map (\stmt -> canInterchange iRanges stmt l1) stmtsBetween) of
              True -> tryToInterchangeAndFuse iRanges (stmtsBetween ++ (fuseLoops l1 l2):rest)
              False -> l1 : (tryToInterchangeAndFuse iRanges (stmtsBetween ++ [l2] ++ rest))
        Nothing -> l1 : (tryToInterchangeAndFuse iRanges stmts)
    False -> l1 : (tryToInterchangeAndFuse iRanges stmts)
tryToInterchangeAndFuse iRanges other = other

nextLoopWithSameIterSpaceAndStmtsBetween _ [] = Nothing
nextLoopWithSameIterSpaceAndStmtsBetween l1 stmts =
  let stmtsBetween = L.takeWhile (\stmt -> (not $ isLoop stmt) || (isLoop stmt && (not $ sameIterationSpace l1 stmt))) stmts in
  case L.length stmtsBetween == L.length stmts of
    True -> Nothing
    False ->
      let loopAndStmtsAfter = L.dropWhile (\stmt -> (not $ isLoop stmt) || (isLoop stmt && (not $ sameIterationSpace l1 stmt))) stmts
          nextLoop = L.head loopAndStmtsAfter
          stmtsAfter = L.tail loopAndStmtsAfter in
      Just (nextLoop, stmtsBetween, stmtsAfter)
