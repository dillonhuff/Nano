module SystemTests.CompactTemps(allCompactTempsTests) where

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
import Transformations.IntroducePacking
import Core.Statement
import TestUtils

allCompactTempsTests = TestLabel "All scalarization system tests" $
                      TestList $ compactTests

compactTests =
  [ltc "matrix add" scalarVarDecls toScalarC compactTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" scalarVarDecls toScalarC compactTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix add" scalarVarDecls toScalarC compactTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" scalarVarDecls toScalarC compactTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" scalarVarDecls toScalarC compactMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix add" scalarVarDecls toScalarC compactTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

compactTempsOpts = (pack 1 "r_"):compactTemps:preprocessingOpts
compactMMulOpts = (pack 1 "r_"):compactTemps:preprocessMMulOpts
compactTransOpts = (pack 1 "r_"):compactTemps:fuseInnerLoops:preprocessTransOpts
