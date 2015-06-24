module Dummies(a, b, c, d, e, f, g, h, i, j,
               x, y, z, p,
               alpha,
               maddCBA, smulCAlphaA, mmulCBA,
               constDblMat) where

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

x = constDblMat "x" 16 1 1 1
y = constDblMat "y" 16 1 1 1
z = constDblMat "z" 16 1 1 1
p = constDblMat "p" 1 16 1 1

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a
mmulCBA = matrixMultiply c b a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)


