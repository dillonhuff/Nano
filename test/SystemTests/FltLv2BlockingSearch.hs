module SystemTests.FltLv2BlockingSearch(allFltLv2BlockingSearchTests,
                                        fltLv2BlockingCasesRowStride,
                                        fltLv2BlockingCasesColStride) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.AVX
import Dummies
import Fuzz
import Operations
import OptimizationGroups.AVXLevel1
import PartitionSearch
import Statement
import TestUtils

allFltLv2BlockingSearchTests =
  TestLabel "All Lv2 blocking search tests" $
  TestList $ [fltLv2BlockingCasesRowStride, fltLv2BlockingCasesColStride]

fltLv2BlockingCasesRowStride =
  TestLabel "Row stride" $ TestList $
  [ltc "row major matrix add" avxVarDecls toAVX lv2Opts [matrixAdd m1 m2 m3],
   ltc "row major matrix add with residual" avxVarDecls toAVX lv2Opts [matrixAdd n1 n2 n3],
   ltc "row major matrix add twice" avxVarDecls toAVX lv2Opts [matrixAdd t1 m1 m2, matrixAdd m3 t1 t1],
   ltc "row major matrix add twice with residual" avxVarDecls toAVX lv2Opts [matrixAdd lt1 l1 l2, matrixAdd l3 lt1 l2],
   ltc "scalar matrix multiply" avxVarDecls toAVX lv2Opts [scalarMultiply m1 alpha m1],
   ltc "scalar matrix multiply with residual" avxVarDecls toAVX lv2Opts [scalarMultiply k1 alpha k2],
   ltc "smul then add" avxVarDecls toAVX lv2Opts [scalarMultiply kt1 alpha k2, matrixAdd k3 k2 kt1],
   ltc "dot product then smul with residual" avxVarDecls toAVX lv2Opts [matrixMultiply alpha x1 y1, scalarMultiply k1 alpha k1],
   ltc "mvmul" avxVarDecls toAVX lv2Opts [matrixMultiply y1 m1 yc1],
   ltc "sgemv" avxVarDecls toAVX lv2Opts (sgemvRM 18 9),
   ltc "sblinf" avxVarDecls toAVX lv2Opts (sblinfRM 24 24),
   ltc "sbigemv" avxVarDecls toAVX lv2Opts (sbigemvRM 8 8),
   ltc "sgemm" avxVarDecls toAVX lv2Opts (sgemmRM 15 17 22),
   ltc "sgemmsum" avxVarDecls toAVX lv2Opts (sgemmsumRM 3 16 9)]

m1 = constFltMat "M1" 8 8 8 1
m2 = constFltMat "M2" 8 8 8 1
m3 = constFltMat "M3" 8 8 8 1

l1 = constFltMat "L1" 19 19 19 1
l2 = constFltMat "L2" 19 19 19 1
l3 = constFltMat "L3" 19 19 19 1

k1 = constFltMat "K1" 17 10 10 1
k2 = constFltMat "K2" 17 10 10 1
k3 = constFltMat "K3" 17 10 10 1

t1 = constFltMatTemp "T1" 8 8 8 1
t2 = constFltMatTemp "T2" 8 8 8 1
lt1 = constFltMatTemp "LT1" 19 19 19 1
kt1 = constFltMatTemp "KT1" 17 10 10 1
tv1 = constFltMatTemp "TV1" 8 1 1 1
tv2 = constFltMatTemp "TV2" 8 1 1 1

n1 = constFltMat "N1" 9 9 9 1
n2 = constFltMat "N2" 9 9 9 1
n3 = constFltMat "N3" 9 9 9 1

x1 = constFltMat "x" 1 8 1 1
y1 = constFltMat "y" 8 1 1 1
yc1 = constFltMat "yc" 8 1 1 1

xu1 = constFltMat "x" 1 11 1 1
yu1 = constFltMat "y" 11 1 1 1

fltLv2BlockingCasesColStride =
  TestLabel "Column stride" $ TestList $
  [ltc "row major matrix add" avxVarDecls toAVX lv2Opts [matrixAdd m1_cs m2_cs m3_cs],
   ltc "row major matrix add with residual" avxVarDecls toAVX lv2Opts [matrixAdd n1_cs n2_cs n3_cs],
   ltc "row major matrix add twice" avxVarDecls toAVX lv2Opts [matrixAdd t1_cs m1_cs m2_cs, matrixAdd m3_cs t1_cs t1_cs],
   ltc "row major matrix add twice with residual" avxVarDecls toAVX lv2Opts [matrixAdd lt1_cs l1_cs l2_cs, matrixAdd l3_cs lt1_cs l2_cs],
   ltc "scalar matrix multiply" avxVarDecls toAVX lv2Opts [scalarMultiply m1_cs alpha m1_cs],
   ltc "scalar matrix multiply with residual" avxVarDecls toAVX lv2Opts [scalarMultiply k1_cs alpha k2_cs],
   ltc "smul then add" avxVarDecls toAVX lv2Opts [scalarMultiply kt1_cs alpha k2_cs, matrixAdd k3_cs k2_cs kt1_cs],
   ltc "dot product then smul with residual" avxVarDecls toAVX lv2Opts [matrixMultiply alpha x1 y1, scalarMultiply k1_cs alpha k1_cs],
   ltc "mvmul" avxVarDecls toAVX lv2Opts [matrixMultiply y1 m1_cs yc1],
   ltc "sgemv" avxVarDecls toAVX lv2Opts (sgemvCM 4 15)]

lv2Opts = (avxLvl1Opts 8) ++ [partitionSearch "b_"]

m1_cs = constFltMat "M1" 8 8 1 8
m2_cs = constFltMat "M2" 8 8 1 8
m3_cs = constFltMat "M3" 8 8 1 8

l1_cs = constFltMat "L1" 19 19 1 19
l2_cs = constFltMat "L2" 19 19 1 19
l3_cs = constFltMat "L3" 19 19 1 19

k1_cs = constFltMat "K1" 17 10 1 17
k2_cs = constFltMat "K2" 17 10 1 17
k3_cs = constFltMat "K3" 17 10 1 17

t1_cs = constFltMatTemp "T1" 8 8 1 8
lt1_cs = constFltMatTemp "LT1" 19 19 1 19
kt1_cs = constFltMatTemp "KT1" 17 10 1 17

n1_cs = constFltMat "N1" 9 9 1 9
n2_cs = constFltMat "N2" 9 9 1 9
n3_cs = constFltMat "N3" 9 9 1 9
