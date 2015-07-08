module SystemTests.Lv2BlockingSearch(allLv2BlockingSearchTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX
import Dummies
import Fuzz
import OptimizationGroups.AVXLevel1
import PartitionSearch
import Statement
import TestUtils

allLv2BlockingSearchTests =
  TestLabel "All Lv2 blocking search tests" $
  TestList $ lv2BlockingCases

lv2BlockingCases =
  [ltc "row major matrix add" avxVarDecls toAVX lv2Opts [matrixAdd m1 m2 m3]]

lv2Opts = avxLvl1Opts ++ [partitionSearch "b_"]

m1 = constDblMat "M1" 8 8 8 1
m2 = constDblMat "M2" 8 8 8 1
m3 = constDblMat "M3" 8 8 8 1

n1 = constDblMat "N1" 9 9 9 1
n2 = constDblMat "N2" 9 9 9 1
n3 = constDblMat "N3" 9 9 9 1

x1 = constDblMat "x" 1 8 1 1
y1 = constDblMat "y" 8 1 1 1

xu1 = constDblMat "x" 1 11 1 1
yu1 = constDblMat "y" 11 1 1 1
