module Dummies(a, b, c, d, e, f, g, h, i, j, k,
               x, y, z, p,
               alpha,
               tr9c9, tr13c4,
               maddCBA, smulCAlphaA, mmulCBA,
               constDblMat,
               blockingOptimizations,
               testOperations) where

import Data.List as L

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

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

constDblMatTemp name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties local double)

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
