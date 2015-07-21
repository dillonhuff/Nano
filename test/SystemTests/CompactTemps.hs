module SystemTests.CompactTemps(allCompactTempsTests) where

import Data.List as L
import Test.HUnit


import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Scalar
import Core.IndexExpression
import Core.Statement
import Dummies
import Fuzz
import TestUtils
import Transformations.Blocking
import Transformations.CompactTemps
import Transformations.Fusion
import Transformations.IntroducePacking
import Transformations.ScalarMMULToFMA

allCompactTempsTests = TestLabel "allCompactTempsTests" $
                      TestList $ compactTests

compactTests =
  [ltc "matrix add" stmtsToScalarC compactTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" stmtsToScalarC compactTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix add" stmtsToScalarC compactTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" stmtsToScalarC compactTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" stmtsToScalarC compactMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix add" stmtsToScalarC compactTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

compactTempsOpts = (pack 1 "r_"):(scalarMMULToFMA "fma"):compactTemps:preprocessingOpts
compactMMulOpts = (pack 1 "r_"):(scalarMMULToFMA "fma"):compactTemps:preprocessMMulOpts
compactTransOpts = (pack 1 "r_"):(scalarMMULToFMA "fma"):compactTemps:fuseInnerLoops:preprocessTransOpts
