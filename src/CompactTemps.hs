module CompactTemps(compactTemps) where

import Data.List as L

import Matrix
import Statement

compactTemps stmts =
  let sspReplacementPairs = computeReplacements stmts in
  L.foldr (\(ssp, newTemp) -> replaceSSPWith ssp newTemp) stmts sspReplacementPairs

computeReplacements stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats in
  L.zip allUnderlyingTempMats allUnderlyingTempMats
      

replaceSSPWith ssp newTemp stmts =
  
