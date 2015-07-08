module Operations(daxpy, ddotsmul, daxpadd,
                  dmvmul, dgemvRM, dgemvCM,
                  dblinfRM, dbigemvRM) where

import Dummies hiding (x, y, z, a, b, c)
import Matrix
import Statement

dbigemvRM m n =
  let alpha = constDblScalar "alpha"
      beta = constDblScalar "beta"
      t1 = constDblColVec "t1" m
      t2 = constDblColVec "t2" m
      x = constDblColVec "x" n
      y = constDblColVec "y" m
      a = constDblMat "A" m n n 1
      b = constDblMat "B" m n n 1 in
  [setZero t1,
   matrixMultiply t1 a x,
   scalarMultiply t1 alpha t1,
   setZero t2,
   matrixMultiply t2 a x,
   scalarMultiply t2 alpha t2,
   matrixAdd y t1 t2]

dblinfRM m n =
  let alpha = constDblScalar "alpha"
      a = constDblMat "A" m n n 1
      t1 = constDblRowVec "t1" m
      t2 = constDblColVec "t2" m
      x = constDblColVec "x" m
      y = constDblColVec "y" n in
  [matrixTranspose t1 x, setZero t2, matrixMultiply t2 a y, matrixMultiply alpha t1 t2]

dgemvRM m n =
  let alpha = constDblScalar "alpha"
      beta = constDblScalar "beta"
      t1 = constDblColVecT "t1" m
      t2 = constDblColVecT "t2" m
      dx = constDblColVec "x" n
      dy = constDblColVec "y" m
      aM = constDblMat "A" m n n 1 in
  [setZero t1, matrixMultiply t1 aM dx, scalarMultiply t2 alpha t1, scalarMultiply dy beta dy, matrixAdd dy t2 dy]

dgemvCM m n =
  let alpha = constDblScalar "alpha"
      beta = constDblScalar "beta"
      t1 = constDblColVecT "t1" m
      t2 = constDblColVecT "t2" m
      dx = constDblColVec "x" n
      dy = constDblColVec "y" m
      aM = constDblMat "A" m n 1 m in
  [setZero t1, matrixMultiply t1 aM dx, scalarMultiply t2 alpha t1, scalarMultiply dy beta dy, matrixAdd dy t2 dy]

dmvmul m n =
  [matrixMultiply (constDblColVec "y" m) (constDblMatRM "A" m n) (constDblColVec "x" n)]

daxpadd n =
  let alpha = constDblScalar "alpha"
      beta = constDblScalar "beta"
      x = constDblColVec "x" n
      y = constDblColVec "y" n
      z = constDblColVec "z" n
      t1 = constDblColVecT "t1" n
      t2 = constDblColVecT "t2" n
      t3 = constDblColVecT "t3" n in
  [scalarMultiply t1 alpha x, matrixAdd t2 y z, scalarMultiply t3 beta t2, matrixAdd y t1 t3]

daxpy n =
  [scalarMultiply (constDblColVecT "t" n) (constDblColVec "alpha" 1) (constDblColVec "x" n),
   matrixAdd (constDblColVec "y" n) (constDblColVecT "t" n) (constDblColVec "y" n)]

ddotsmul n =
  [matrixTranspose (constDblColVecT "zt" n) (constDblRowVec "z" n),
   matrixMultiply (constDblScalar "alpha") (constDblRowVec "z" n) (constDblColVecT "zt" n),
   scalarMultiply (constDblColVec "x" n) (constDblScalar "alpha") (constDblColVec "x" n)]

constDblScalar name =
  constDblColVec name 1

constDblColVec name nr =
  constDblMat name nr 1 1 1

constDblColVecT name nr =
  constDblMatTemp name nr 1 1 1

constDblRowVec name nc =
  constDblMat name 1 nc 1 1

constDblRowVecT name nc =
  constDblMatTemp name 1 nc 1 1

constDblMatRM name nr nc =
  constDblMat name nr nc nc 1
