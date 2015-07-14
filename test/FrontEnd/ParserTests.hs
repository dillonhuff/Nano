module FrontEnd.ParserTests(allParserTests) where

import Data.List as L
import Data.Map as M
import Test.HUnit

import Dummies
import Core.IndexExpression
import Core.Matrix
import Core.MatrixOperation
import Core.MemLocation
import FrontEnd.Lexer
import FrontEnd.Parser
import FrontEnd.Token
import Module

allParserTests = TestLabel "allParserTests" $ TestList 
   [makeTestCases (lexAndParseStatement "noname.lspc" matMap) stCases,
    makeTestCases (lexAndParseFormalParam "noname.lspc") formalParamCases,
    makeTestCases (lexAndParseOperation "noname.lspc") opCases]

opCases =
  L.map (\(x, y) -> (x, Right y))
  [("operation nothing() { }",
    matrixOperation "nothing" [] dummyPos),
   ("operation oneOp(oarg scal single a) {}",
    matrixOperation "oneOp" [] dummyPos),
   ("operation sscal(oarg cvec 12 single x, \niarg scal single alpha) { \nx = alpha .* x;\n }",
    matrixOperation "sscal" [scalX] dummyPos)]

scalX = dmasg (dmat (constFltMat "x" 12 1 1 1))
              (dmBinop SMul
                       (dmat (constFltMat "alpha" 1 1 1 1))
                       (dmat (constFltMat "x" 12 1 1 1)))

stCases =
  L.map (\(x, y) -> (x, Right y))
  [("C = A + B;", dmasg (dmat c) (dmBinop MAdd (dmat a) (dmat b))),
   ("C = A * B;", dmasg (dmat c) (dmBinop MMul (dmat a) (dmat b))),
   ("C = alpha .* B;", dmasg (dmat c) (dmBinop SMul (dmat alpha) (dmat b))),
   ("A = (alpha.*C) + A';",
    dmasg (dmat a) (dmBinop MAdd (dmBinop SMul (dmat alpha) (dmat c)) (dmUnop MTrans (dmat a))))]

formalParamCases =
  L.map (\(x, y) -> (x, Right y))
  [("iarg scal double c", ("c", constDblMat "c" 1 1 1 1)),
   ("temp scal double c", ("c", constDblMatTemp "c" 1 1 1 1)),
   ("oarg scal single c", ("c", constFltMat "c" 1 1 1 1)),
   ("oarg rvec 12 double xVec", ("xVec", constDblMat "xVec" 1 12 1 1)),
   ("iarg cvec 19 single yVec", ("yVec", constFltMat "yVec" 19 1 1 1)),
   ("iarg cvec n double yVec",
    ("yVec", argDblMat "yVec" (iVar "n") (iConst 1) (iConst 1) (iConst 1))),
   ("iarg matx rm 12 34 single A",
    ("A", constFltMat "A" 12 34 34 1)),
   ("iarg matx cm m n single A",
    ("A", argFltMat "A" (iVar "m") (iVar "n") (iConst 1) (iVar "m")))]

argDblMat n nr nc rs cs =
  matrix n nr nc rs cs (properties arg double memory)

argFltMat n nr nc rs cs =
  matrix n nr nc rs cs (properties arg single memory)

lexAndParseStatement fName matMap str =
  (lexString fName str) >>= (parseStatement fName matMap)
lexAndParseFormalParam fName str = (lexString fName str) >>= (parseFormalParam fName)
lexAndParseOperation fName str = (lexString fName str) >>= (parseOperation fName)

matMap =
  M.fromList matList

matList =
  [("A", a), ("B", b), ("C", c), ("alpha", alpha)]
