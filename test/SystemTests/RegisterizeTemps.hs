module SystemTests.RegisterizeTemps(allRegisterizeTempsTests) where

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
import Transformations.RegisterizeTemps
import Transformations.IntroducePacking
import Transformations.ScalarMMULToFMA

allRegisterizeTempsTests = TestLabel "allRegisterizeTempsTests" $
                      TestList $ registerizeTests

registerizeTests =
  [ltc "matrix add" scalarVarDecls stmtsToScalarC registerizeTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" scalarVarDecls stmtsToScalarC registerizeTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix adds" scalarVarDecls stmtsToScalarC registerizeTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" scalarVarDecls stmtsToScalarC registerizeTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" scalarVarDecls stmtsToScalarC registerizeMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix multiply" scalarVarDecls stmtsToScalarC registerizeTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

registerizeTempsOpts = (registerizeTemps 1):(pack 1 "r_"):(scalarMMULToFMA "fma"):compactTemps:preprocessingOpts
registerizeMMulOpts = (registerizeTemps 1):(pack 1 "r_"):(scalarMMULToFMA "fma"):compactTemps:preprocessMMulOpts
registerizeTransOpts = (registerizeTemps 1):(pack 1 "r_"):(scalarMMULToFMA "fma"):compactTemps:fuseInnerLoops:preprocessTransOpts
