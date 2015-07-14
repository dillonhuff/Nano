module SystemTests.RegisterizeTemps(allRegisterizeTempsTests) where

import Data.List as L
import Test.HUnit

import Transformations.Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Scalar
import Transformations.CompactTemps
import Dummies
import Transformations.Fusion
import Fuzz
import Core.IndexExpression
import Transformations.RegisterizeTemps
import Transformations.IntroducePacking
import Core.Statement
import TestUtils

allRegisterizeTempsTests = TestLabel "All scalarization system tests" $
                      TestList $ registerizeTests

registerizeTests =
  [ltc "matrix add" scalarVarDecls toScalarC registerizeTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" scalarVarDecls toScalarC registerizeTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix adds" scalarVarDecls toScalarC registerizeTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" scalarVarDecls toScalarC registerizeTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" scalarVarDecls toScalarC registerizeMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix multiply" scalarVarDecls toScalarC registerizeTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

registerizeTempsOpts = (pack 1 "r_"):(registerizeTemps 1):compactTemps:preprocessingOpts
registerizeMMulOpts = (pack 1 "r_"):(registerizeTemps 1):compactTemps:preprocessMMulOpts
registerizeTransOpts = (pack 1 "r_"):(registerizeTemps 1):compactTemps:fuseInnerLoops:preprocessTransOpts
