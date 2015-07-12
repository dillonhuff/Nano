module Benchmarking(timeOperationGS) where

import Data.List as L
import Test.HUnit

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Core
import CBackEnd.CodeGeneration.Function
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import CBackEnd.Utils
import MatrixOperation
import Statement
import Utils

optimizeOp matStmtOpts stmtOpts matStmts =
  error "optimizeOp"

timeOperationGS dimVals matrixStmtOpts stmtOpts varDeclFunc codeGenFunc operation =
  let matStmts = matOpBody operation
      optimizedOp = optimizeOp matrixStmtOpts stmtOpts matStmts
      (optimizedC, bufsAndIVars) = operationToC varDeclFunc codeGenFunc "optimizedOp" optimizedOp in
  timeOperationWithDimVals dimVals optimizedC (timingCall optimizedC bufsAndIVars)

timingCall f args =
  cExprSt (cFuncall (cFuncName f) (L.map (cVar . bufName) args)) ""

timeOperationWithDimVals dimVals func fCall =
  error "timeOperationWithDimVals"

{-  let (transformedOp, _) = operationToC varDeclFunc codeGenFunc "transformedOp" $ applyOptimizations transformsToApply operation
      (regularOp, bufsAndIVars) = operationToC scalarVarDecls toCStmtsFunction "op" operation
      argInfo = L.takeWhile (\b -> isCPtr $ bufType b) bufsAndIVars
      indInfo = L.dropWhile (\b -> isCPtr $ bufType b) bufsAndIVars
      indDecls = bufDecls indInfo
      indNames = L.map bufName indInfo
      indVars = L.map cVar indNames
      indInits = L.map (\indVar -> cExprSt (cAssign indVar (cIntLit 79)) "") indVars
      scFuncall = \bufs -> [cExprSt (cFuncall (cFuncName regularOp) ((L.map (cVar . bufName) bufs) ++ indVars)) ""]
      testFuncall = \bufs -> [cExprSt (cFuncall (cFuncName transformedOp) ((L.map (cVar . bufName) bufs) ++ indVars)) ""] in
    do
      scRes <- runSanityCheckGS "gs_timing" indDecls indInits scFuncall testFuncall regularOp transformedOp argInfo
      putStrLn scRes
-}
