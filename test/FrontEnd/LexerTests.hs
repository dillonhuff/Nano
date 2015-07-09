module FrontEnd.LexerTests(allLexerTests) where

import Data.List as L
import Test.HUnit

import FrontEnd.Lexer
import Module
import FrontEnd.Token

allLexerTests = TestLabel "All lexer tests" $ TestList $
  [makeTestCases (lexString "nofile.lspc") keywordCases,
   makeTestCases (lexString "nofile.lspc") identCases,
   makeTestCases (lexString "nofile.lspc") litCases]

keywordCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("architecture", dres "architecture"),
   ("operation", dres "operation"),
   ("mat", dres "mat"),
   ("vec", dres "vec"),
   ("iarg", dres "iarg"),
   ("oarg", dres "oarg"),
   ("temp", dres "temp"),
   ("LangC", dres "LangC"),
   ("rm", dres "rm"),
   ("cm", dres "cm"),
   ("double", dres "double"),
   ("sca", dres "sca"),
   ("single", dres "single"),
   ("{", dres "{"),
   ("}", dres "}"),
   ("(", dres "("),
   (")", dres ")"),
   (",", dres ","),
   ("=", dres "="),
   (";", dres ";"),
   ("+", dres "+"),
   ("-", dres "-"),
   ("*", dres "*"),
   (".*", dres ".*"),
   ("'", dres "'")]

identCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("arc", dident "arc"),
   ("cmoutput", dident "cmoutput"),
   ("casey12", dident "casey12"),
   ("ROGERTHAT", dident "ROGERTHAT")]
  
litCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("12", dIntLit 12)]
