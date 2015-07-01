module Dummies(a, b, c, d, e, f, g, h, i, j, k,
               x, y, z, p,
               alpha,
               tr9c9, tr13c4,
               fA, fB, fC,
               dummyRanges,
               maddCBA, smulCAlphaA, mmulCBA,
               constDblMat, constRect,
               blockingOptimizations,
               testOperations, compoundTestOperations) where

import Data.List as L
import Data.Map as M

import Analysis.IndexExpression
import Blocking
import IndexExpression
import Matrix
import Statement

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 9 7 9
c = constDblMat "C" 9 9 9 1
d = constDblMat "D" 13 9 1 13
e = constDblMat "E" 9 4 4 19
f = constDblMat "F" 13 4 4 1
g = constDblMat "G" 13 4 1 13
h = constDblMat "H" 13 4 1 13
i = constDblMat "I" 4 13 13 1
j = constDblMat "J" 16 16 1 16
k = constDblMat "K" 13 13 1 13

x = constDblMat "x" 16 1 1 1
y = constDblMat "y" 16 1 1 1
z = constDblMat "z" 16 1 1 1
p = constDblMat "p" 1 16 1 1

alpha = constDblMat "alpha" 1 1 1 1

tr9c9 = constDblMatTemp "tr9c9" 9 9 1 9
tr13c4 = constDblMatTemp "tr13c4" 13 4 1 13

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a
mmulCBA = matrixMultiply c b a

fA = constFltMat "fA" 123 57 57 1
fB = constFltMat "fB" 123 57 57 1
fC = constFltMat "fC" 123 57 57 1

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double memory)

constFltMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg single memory)

constDblMatTemp name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties local double memory)

blockingOptimizations =
  L.map (\(f, b) -> blkUniqueVar f b)
  [(blockMatrixAddM, iConst 1),
   (blockMatrixAddN, iConst 1),
   (blockMatrixAddM, iConst 3),
   (blockMatrixAddN, iConst 11),
   (blockScalarMultiplyM, iConst 1),
   (blockScalarMultiplyN, iConst 3),
   (blockScalarMultiplyN, iConst 4),
   (blockMatrixMultiplyM, iConst 1),
   (blockMatrixMultiplyN, iConst 1),
   (blockMatrixMultiplyP, iConst 1),
   (blockMatrixTransposeM, iConst 1),
   (blockMatrixTransposeM, iConst 6),
   (blockMatrixTransposeN, iConst 3),
   (blockMatrixTransposeN, iConst 1)]

testOperations =
   [[matrixAdd f g h],
    [matrixAdd f f f],
    [matrixAdd g h f],
    [matrixAdd a b c],
    [scalarMultiply a alpha a],
    [matrixMultiply a b c],
    [matrixMultiply a b b],
    [matrixMultiply h d e],
    [matrixTranspose h i],
    [matrixTranspose i h],
    [matrixMultiply x j y],
    [matrixMultiply alpha p x],
    [matrixMultiply j x p],
    [matrixAdd tr9c9 a b,
     matrixAdd c tr9c9 c]]

compoundTestOperations =
  [[matrixAdd a b c],
   [matrixAdd c c c, matrixAdd a a a],
   [scalarMultiply a alpha a, matrixMultiply c a b]]
--   [scalarMultiply a alpha a, matrixAdd tr9c9 a tr9c9, matrixMultiply c tr9c9 b]]
--   [scalarMultiply a alpha a, matrixTranspose tr9c9 a, matrixMultiply c tr9c9 tr9c9]]
  
blkUniqueVar :: (IExpr -> IExpr -> Statement -> [Statement]) -> IExpr -> [Statement] -> [Statement]
blkUniqueVar blkFunc blkFactor stmts =
  let v = uniqueVarName stmts in
  expandStatementsBU (blkFunc v blkFactor) stmts

uniqueVarName stmts =
  let vs = uniqueVars stmts in
  case vs of
    [] -> iVar "i"
    vs ->iVar $ (varName $ L.head $ L.sortBy (\a b -> compare b a) vs) ++ "z"

uniqueVars stmts = L.nub $ L.concatMap (collectValuesFromStmt loopIVar) stmts

loopIVar stmt =
  case isLoop stmt of
    True -> [iVar $ loopInductionVariable stmt]
    False -> []

dummyRanges :: Map IExpr (IExpr, IExpr, IExpr)
dummyRanges = M.fromList [(iVar "i", (iConst 0, iConst 2, iConst 15)),
                          (iVar "j", (iConst 0, iConst 3, iConst 17)),
                          (iVar "k", (iConst 0, iConst 1, iConst 8)),
                          (iVar "i1", (iConst 0, iConst 6, iConst 4))]

constRect :: Int -> Int -> Int -> Int -> IRectangle
constRect r1 c1 r2 c2 =
  iRectangle (iRange (iConst r1) (iConst c1)) (iRange (iConst r2) (iConst c2))
