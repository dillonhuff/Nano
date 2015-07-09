module AllModuleTests(allModuleTests) where

import Test.HUnit

import Analysis.IndexExpressionTests
import Analysis.MatrixTests
import BlockingTests
import FrontEnd.LexerTests
import FrontEnd.ParserTests
import FusionTests
import IndexExpressionTests
import MatrixTests
import StatementInterchangeTests

allModuleTests = TestLabel "All module tests" $ TestList
               [Analysis.IndexExpressionTests.allIndexExpressionTests,
                Analysis.MatrixTests.allMatrixTests,
                allBlockingTests,
                allFusionTests,
                IndexExpressionTests.allIndexExpressionTests,
                MatrixTests.allMatrixTests,
                allLexerTests,
                allParserTests
                allStatementInterchangeTests]
