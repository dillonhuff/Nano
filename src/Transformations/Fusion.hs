module Transformations.Fusion(canFuseIfAdjacent,
              fuseInnerLoops, fuseLoops) where

import Data.List as L

import Analysis.Loop
import Core.IndexExpression
import Core.Matrix
import Core.Statement

fuseInnerLoops :: [Statement] -> [Statement]
fuseInnerLoops stmts = applyToLoopBodiesBU tryToFuseStmtList stmts

tryToFuseStmtList [] = []
tryToFuseStmtList [stmt] = [stmt]
tryToFuseStmtList (l:r:rest) =
  case canFuseIfAdjacent l r of
    True -> tryToFuseStmtList ((fuseLoops l r):rest)
    False -> l : (tryToFuseStmtList (r:rest))

canFuseIfAdjacent :: Statement -> Statement -> Bool
canFuseIfAdjacent first second =
  case isInnerLoop first && isInnerLoop second of
    True -> case sameIterationSpace first second of
      True -> fusionDoesNotCreateDependencesFromFirstLoopToSecond first second
      False -> False
    False -> False

fusionDoesNotCreateDependencesFromFirstLoopToSecond :: Statement -> Statement -> Bool
fusionDoesNotCreateDependencesFromFirstLoopToSecond first second =
  let fusedBody = loopBody $ fuseLoops first second
      firstBody = L.take (length $ loopBody first) fusedBody
      secondBody = L.drop (length $ loopBody first) fusedBody in
  L.null $ L.filter (\(s, t) -> dependsOnLexicallyBefore s t) [(s, t) | s <- firstBody, t <- secondBody]

fuseLoops :: Statement -> Statement -> Statement
fuseLoops first second =
  loop (loopInductionVariable first) (loopStart first) (loopInc first) (loopEnd first) fusedBody
  where
    i = iVar $ loopInductionVariable second
    j = iVar $ loopInductionVariable first
    secondFused = L.map (applyToStatementBU (applyToOperands (substituteInIExprs i j))) $ loopBody second
    fusedBody = (loopBody first) ++ secondFused

dependsOnLexicallyBefore :: Statement -> Statement -> Bool
dependsOnLexicallyBefore s t =
  case matricesInCommon s t of
    [] -> False
    commonMatrixPairs -> L.or $ L.map (\(l, r) -> potentialNegativeDependenceVector l r) commonMatrixPairs

potentialNegativeDependenceVector l r =
  (not (l == r)) || (l == r && (partitionList l == []) && (partitionList r == []))

matricesInCommon :: Statement -> Statement -> [(Matrix, Matrix)]
matricesInCommon s t =
  L.filter (\(l, r) -> bufferName l == bufferName r) [(l, r) | l <- allOperands s, r <- allOperands t]
