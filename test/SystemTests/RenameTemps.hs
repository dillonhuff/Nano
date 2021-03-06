module SystemTests.RenameTemps(allRenameTempsTests) where

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

registerizeTests =
  [ltc "matrix add" stmtsToScalarC registerizeTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" stmtsToScalarC registerizeTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix adds" stmtsToScalarC registerizeTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" stmtsToScalarC registerizeTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" stmtsToScalarC registerizeMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix multiply" stmtsToScalarC registerizeTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

registerizeTempsOpts = (registerize 1 "r_"):(registerizeTemps 1):compactTemps:preprocessingOpts
registerizeMMulOpts = (registerize 1 "r_"):(registerizeTemps 1):compactTemps:preprocessMMulOpts
registerizeTransOpts = (registerize 1 "r_"):(registerizeTemps 1):compactTemps:fuseInnerLoops:preprocessTransOpts

allRenameTempsTests =
  TestLabel "All rename temps tests" $
  TestList renameTempsCases

renameTempsCases =
  []
