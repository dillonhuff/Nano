module TestUtils(ltc) where

import Test.HUnit

import Fuzz

ltc testName varDeclFunc codeGenFunc optimizations operation =
  TestLabel testName $ TestCase $ assertOptimizationsCorrect varDeclFunc codeGenFunc optimizations operation
