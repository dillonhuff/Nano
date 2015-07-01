module CompactTemps(compactTemps) where

import Data.List as L
import Data.Maybe
import Data.Foldable

import Matrix
import Statement

compactTemps stmts =
  let sspReplacementPairs = computeReplacements stmts in
  L.foldr (\(ssp, newTemp) -> replaceSSPWith ssp newTemp) stmts sspReplacementPairs

computeReplacements :: [Statement] -> [(Matrix, Matrix)]
computeReplacements stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats
      ssps = L.map (\m -> computeSSP m stmts) allUnderlyingTempMats in
  L.map (\(l, r) -> (l, fromJust r)) $ L.filter sndIsJust $ L.zip allUnderlyingTempMats ssps

sndIsJust (l, r) =
  case r of
    Just _ -> True
    Nothing -> False

replaceSSPWith ssp newTemp stmts =
  expandStatementsBU (\st -> [applyToOperands (replaceSupermatrix ssp newTemp) st]) stmts

computeSSP m stmts =
  let allMats = L.concatMap (collectValuesFromStmt allOperands) stmts
      allRefs = L.filter (\refM -> bufferName refM == bufferName m) allMats in
  foldrM smallestSubsumingPartitionLR (L.head allRefs) (L.tail allRefs)

smallestSubsumingPartitionLR m n =
  case smallestSubsumingPartition m n of
    Just a -> Just a
    Nothing -> case smallestSubsumingPartition n m of
      Just b -> Just b
      Nothing -> Nothing
