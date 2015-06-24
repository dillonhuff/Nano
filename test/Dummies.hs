module Dummies(a, b, c, d, e, f, g, h, i, alpha,
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

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a
mmulCBA = matrixMultiply c b a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)


