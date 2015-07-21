module TestUtils(ltc) where

import Test.HUnit

import Fuzz

ltc testName codeGenFunc optimizations operation =
  TestLabel testName $ TestCase $ assertOptimizationsCorrect codeGenFunc optimizations operation
