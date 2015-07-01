module SystemTests.CompactTemps(allCompactTempsTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import Scalarization
import Statement
import TestUtils

allCompactTempsTests = TestLabel "All scalarization system tests" $
                      TestList $ compactTests

compactTests =
  [ltc "matrix add" compactTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" compactTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix add" compactTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" compactTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" compactMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix add" compactTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

compactTempsOpts = (scalarize "r_"):compactTemps:preprocessingOpts
compactMMulOpts = (scalarize "r_"):compactTemps:preprocessMMulOpts
compactTransOpts = (scalarize "r_"):compactTemps:fuseInnerLoops:preprocessTransOpts
