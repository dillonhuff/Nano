module SystemTests.MultiBlocking(allMultiBlockingTests) where

import Data.List as L
import Control.Monad.Random
import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.Function
import Dummies
import Fuzz
import IndexExpression
import Statement

allMultiBlockingTests = TestLabel "All multi blocking tests" $
                        TestList $ L.map (\op -> TestCase $ assertRandomOptimizationsCorrect toCStmtsFunction blockingOptimizations op) testOperations
