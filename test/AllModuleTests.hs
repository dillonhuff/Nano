module AllModuleTests(allModuleTests) where

import Test.HUnit

import BlockingTests
import IndexExpressionTests
import MatrixTests

allModuleTests = TestLabel "All module tests" $ TestList
               [allBlockingTests,
                allIndexExpressionTests,
                allMatrixTests]
