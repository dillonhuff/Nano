module Transformations.SplitTemps(splitTemps) where

import Data.List as L

import Analysis.Matrix
import Matrix
import Statement
import Transformations.StatementInterchange

splitTemps :: String -> [Statement] -> [Statement]
splitTemps prefix stmts =
  let allTempOps = L.filter (\m -> bufferScope m == local) $ L.concatMap (collectFromAllOperands id) stmts
      varRanges = iVarRanges stmts
      tempGroups = groupByOverlap varRanges allTempOps
      replacements = makeReplacements prefix tempGroups in
  L.foldr replaceOpName stmts replacements

groupByOverlap iRanges ms =
  L.foldr (mergeGroups iRanges) (L.map (\m -> m:[]) ms) ms

mergeGroups iRanges m groups =
  let groupsToMerge = L.filter (\g -> L.any (\gmat -> matricesOverlap iRanges m gmat) g) groups
      groupsToStaySame = groups \\ groupsToMerge in
  (L.concat groupsToMerge):groupsToStaySame

makeReplacements :: String -> [[Matrix]] -> [(Matrix, String)]
makeReplacements prefix matGroups =
  let matGroupByIndex = L.zip [1..(length matGroups)] matGroups in
  L.concatMap (\(i, mats) -> L.map (\m -> (m, prefix ++ show i)) mats) matGroupByIndex

replaceOpName :: (Matrix, String) -> [Statement] -> [Statement]
replaceOpName (m, newName) stmts =
  expandStatementsBU (\st -> [applyToOperands (\mat -> if mat == m then setName newName mat else mat) st]) stmts
