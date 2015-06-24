module Dummies(a, b, c, d, e, f, g, h, i, j, k,
               x, y, z, p,
               alpha,
               tr9c9, tr13c4,
               maddCBA, smulCAlphaA, mmulCBA,
               constDblMat,
               blockingTransforms, blockingOptimizations,
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
  L.map (\t -> expandStatementsBU t) blockingTransforms
  
blockingTransforms =
  [blockMatrixAddM (iVar "i1") (iConst 1),
   blockMatrixAddN (iVar "i2") (iConst 1),
   blockMatrixAddM (iVar "i3") (iConst 3),
   blockMatrixAddN (iVar "i4") (iConst 11),
   blockScalarMultiplyM (iVar "i5") (iConst 1),
   blockScalarMultiplyN (iVar "i6") (iConst 3),
   blockScalarMultiplyN (iVar "i7") (iConst 4),
   blockMatrixMultiplyM (iVar "i8") (iConst 1),
   blockMatrixMultiplyN (iVar "i9") (iConst 1),
   blockMatrixMultiplyP (iVar "i10") (iConst 1),
   blockMatrixTransposeM (iVar "i11") (iConst 1),
   blockMatrixTransposeM (iVar "i12") (iConst 6),
   blockMatrixTransposeN (iVar "i13") (iConst 3),
   blockMatrixTransposeN (iVar "i14") (iConst 1)]   

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
