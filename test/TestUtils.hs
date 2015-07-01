module TestUtils(ltc) where

import Test.HUnit

import Fuzz

ltc testName optimizations operation =
  TestLabel testName $ TestCase $ assertOptimizationsCorrect optimizations operation
