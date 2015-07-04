module SystemTests.RenameTemps(allRenameTempsTests) where

module SystemTests.RegisterizeTemps(allRegisterizeTempsTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Scalar
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import RegisterizeTemps
import Registerization
import Statement
import TestUtils

registerizeTests =
  [ltc "matrix add" scalarVarDecls toScalarC registerizeTempsOpts [matrixAdd a b c],
   ltc "two matrix adds" scalarVarDecls toScalarC registerizeTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   ltc "second two matrix adds" scalarVarDecls toScalarC registerizeTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   ltc "scalar multiply then matrix add" scalarVarDecls toScalarC registerizeTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" scalarVarDecls toScalarC registerizeMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "matrix transpose then matrix multiply" scalarVarDecls toScalarC registerizeTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

registerizeTempsOpts = (registerize 1 "r_"):(registerizeTemps 1):compactTemps:preprocessingOpts
registerizeMMulOpts = (registerize 1 "r_"):(registerizeTemps 1):compactTemps:preprocessMMulOpts
registerizeTransOpts = (registerize 1 "r_"):(registerizeTemps 1):compactTemps:fuseInnerLoops:preprocessTransOpts

allRenameTempsTests =
  TestLabel "All rename temps tests" $
  TestList renameTempsCases

renameTempsCases =
  []
