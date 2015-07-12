module CostModel.FlopsPlusTempAllocs(flopsPlusTempAllocs) where

import Data.List as L

import CostModel.Flops
import Core.Matrix
import Core.Statement

flopsPlusTempAllocs stmts = (flopCost stmts) + (tempAllocCost stmts)

tempAllocCost stmts =
  (fromIntegral $ numTempBuffersAllocated stmts) * tempAllocationCost

tempAllocationCost = 1000.0

numTempBuffersAllocated stmts =
    let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
        allUnderlyingTempMats = L.filter (\m -> not $ isRegister m && bufferScope m == local) allUnderlyingMats in
    L.length allUnderlyingTempMats
