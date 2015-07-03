module SystemTests.RegisterizeTemps(allRegisterizeTempsTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.Scalar
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import RegisterizeTemps
import Scalarization
import Statement
import TestUtils

allRegisterizeTempsTests = TestLabel "All scalarization system tests" $
                      TestList $ registerizeTests

registerizeTests =
  [ltc "matrix add" toScalarC registerizeTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" toScalarC registerizeTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix adds" toScalarC registerizeTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" toScalarC registerizeTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" toScalarC registerizeMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix multiply" toScalarC registerizeTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

registerizeTempsOpts = (scalarize "r_"):registerizeTemps:compactTemps:preprocessingOpts
registerizeMMulOpts = (scalarize "r_"):registerizeTemps:compactTemps:preprocessMMulOpts
registerizeTransOpts = (scalarize "r_"):registerizeTemps:compactTemps:fuseInnerLoops:preprocessTransOpts
