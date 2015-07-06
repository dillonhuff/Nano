module Operations(daxpy, ddotsmul) where

import Dummies
import Matrix
import Statement

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


