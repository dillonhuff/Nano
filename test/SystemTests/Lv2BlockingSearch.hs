module SystemTests.Lv2BlockingSearch(allLv2BlockingSearchTests,
                                     lv2BlockingCasesRowStride,
                                     lv2BlockingCasesColStride) where

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
  TestList $ [lv2BlockingCasesRowStride, lv2BlockingCasesColStride]

lv2BlockingCasesRowStride =
  TestLabel "Row stride" $ TestList $
  [ltc "row major matrix add" avxVarDecls toAVX lv2Opts [matrixAdd m1 m2 m3],
   ltc "row major matrix add with residual" avxVarDecls toAVX lv2Opts [matrixAdd n1 n2 n3],
   ltc "row major matrix add twice" avxVarDecls toAVX lv2Opts [matrixAdd t1 m1 m2, matrixAdd m3 t1 t1],
   ltc "row major matrix add twice with residual" avxVarDecls toAVX lv2Opts [matrixAdd lt1 l1 l2, matrixAdd l3 lt1 l2],
   ltc "scalar matrix multiply" avxVarDecls toAVX lv2Opts [scalarMultiply m1 alpha m1],
   ltc "scalar matrix multiply with residual" avxVarDecls toAVX lv2Opts [scalarMultiply k1 alpha k2],
   ltc "smul then add" avxVarDecls toAVX lv2Opts [scalarMultiply kt1 alpha k2, matrixAdd k3 k2 kt1],
   ltc "dot product then smul with residual" avxVarDecls toAVX lv2Opts [matrixMultiply alpha x1 y1, scalarMultiply k1 alpha k1],
   ltc "mvmul" avxVarDecls toAVX lv2Opts [matrixMultiply y1 m1 yc1]]

m1 = constDblMat "M1" 8 8 8 1
m2 = constDblMat "M2" 8 8 8 1
m3 = constDblMat "M3" 8 8 8 1

l1 = constDblMat "L1" 19 19 19 1
l2 = constDblMat "L2" 19 19 19 1
l3 = constDblMat "L3" 19 19 19 1

k1 = constDblMat "K1" 17 10 10 1
k2 = constDblMat "K2" 17 10 10 1
k3 = constDblMat "K3" 17 10 10 1

t1 = constDblMatTemp "T1" 8 8 8 1
lt1 = constDblMatTemp "LT1" 19 19 19 1
kt1 = constDblMatTemp "KT1" 17 10 10 1

n1 = constDblMat "N1" 9 9 9 1
n2 = constDblMat "N2" 9 9 9 1
n3 = constDblMat "N3" 9 9 9 1

x1 = constDblMat "x" 1 8 1 1
y1 = constDblMat "y" 8 1 1 1
yc1 = constDblMat "yc" 8 1 1 1

xu1 = constDblMat "x" 1 11 1 1
yu1 = constDblMat "y" 11 1 1 1

lv2BlockingCasesColStride =
  TestLabel "Column stride" $ TestList $
  [ltc "row major matrix add" avxVarDecls toAVX lv2Opts [matrixAdd m1_cs m2_cs m3_cs],
   ltc "row major matrix add with residual" avxVarDecls toAVX lv2Opts [matrixAdd n1_cs n2_cs n3_cs],
   ltc "row major matrix add twice" avxVarDecls toAVX lv2Opts [matrixAdd t1_cs m1_cs m2_cs, matrixAdd m3_cs t1_cs t1_cs],
   ltc "row major matrix add twice with residual" avxVarDecls toAVX lv2Opts [matrixAdd lt1_cs l1_cs l2_cs, matrixAdd l3_cs lt1_cs l2_cs],
   ltc "scalar matrix multiply" avxVarDecls toAVX lv2Opts [scalarMultiply m1_cs alpha m1_cs],
   ltc "scalar matrix multiply with residual" avxVarDecls toAVX lv2Opts [scalarMultiply k1_cs alpha k2_cs],
   ltc "smul then add" avxVarDecls toAVX lv2Opts [scalarMultiply kt1_cs alpha k2_cs, matrixAdd k3_cs k2_cs kt1_cs],
   ltc "dot product then smul with residual" avxVarDecls toAVX lv2Opts [matrixMultiply alpha x1 y1, scalarMultiply k1_cs alpha k1_cs],
   ltc "mvmul" avxVarDecls toAVX lv2Opts [matrixMultiply y1 m1_cs yc1]]

lv2Opts = avxLvl1Opts ++ [partitionSearch "b_"]

m1_cs = constDblMat "M1" 8 8 1 8
m2_cs = constDblMat "M2" 8 8 1 8
m3_cs = constDblMat "M3" 8 8 1 8

l1_cs = constDblMat "L1" 19 19 1 19
l2_cs = constDblMat "L2" 19 19 1 19
l3_cs = constDblMat "L3" 19 19 1 19

k1_cs = constDblMat "K1" 17 10 1 17
k2_cs = constDblMat "K2" 17 10 1 17
k3_cs = constDblMat "K3" 17 10 1 17

t1_cs = constDblMatTemp "T1" 8 8 1 8
lt1_cs = constDblMatTemp "LT1" 19 19 1 19
kt1_cs = constDblMatTemp "KT1" 17 10 1 17

n1_cs = constDblMat "N1" 9 9 1 9
n2_cs = constDblMat "N2" 9 9 1 9
n3_cs = constDblMat "N3" 9 9 1 9
