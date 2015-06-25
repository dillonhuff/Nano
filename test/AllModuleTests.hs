module AllModuleTests(allModuleTests) where

import Test.HUnit

import BlockingTests
import FusionTests
import IndexExpressionTests
import MatrixTests

allModuleTests = TestLabel "All module tests" $ TestList
               [allBlockingTests,
                allFusionTests,
                allIndexExpressionTests,
                allMatrixTests]
