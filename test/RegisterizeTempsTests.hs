module RegisterizeTempsTests(allRegisterizeTempsTests) where

import Data.List as L
import Test.HUnit

import Core.Matrix
import Core.Statement
import Module
import Operations
import OptimizationGroups.AVXLevel1
import Utils

allRegisterizeTempsTests = TestLabel "allRegisterizeTempsTests" $ TestList
  [makeTestCases countInMemTemps level1Cases]

level1Cases =
  L.map (\(stmts, n) -> (applyOptimizations (avxLvl1Opts 4) stmts, n))
  [(ddotsmul 16, 0),
   (ddotsmul 19, 0),
   (daxpy 16, 0),
   (daxpy 17, 0),
   (daxpadd 19, 0)]

countInMemTemps op =
  L.length (inMemoryTemps op)

inMemoryTemps stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats
      allInMemTemps = L.filter (\m -> not $ isRegister m) allUnderlyingTempMats in
  allInMemTemps
