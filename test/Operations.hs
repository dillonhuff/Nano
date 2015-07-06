module Operations(daxpy, ddotsmul, daxpadd,
                  dmvmul) where

import Dummies
import Matrix
import Statement

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
