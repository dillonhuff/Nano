module Benchmarking(timeOperationGS) where

import Data.List as L
import Test.HUnit

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Core
import CBackEnd.CodeGeneration.Function
import CBackEnd.SanityCheck
import CBackEnd.Syntax
import CBackEnd.Timing
import CBackEnd.Utils
import Core.MatrixOperation
import Core.Statement
import Utils

optimizeOp matStmtOpts stmtOpts matStmts =
  applyOptimizations stmtOpts $ linearizeStmts "TQ_" $ applyOptimizations matStmtOpts matStmts

timeOperationGS dimVals matrixStmtOpts stmtOpts varDeclFunc codeGenFunc operation =
  let matStmts = matOpBody operation
      optimizedOp = optimizeOp matrixStmtOpts stmtOpts matStmts
      (optimizedC, bufsAndIVars) = operationToC varDeclFunc codeGenFunc "optimizedOp" optimizedOp in
  timeOperationWithDimVals dimVals bufsAndIVars optimizedC (timingCall optimizedC bufsAndIVars)

timingCall f args =
  cExprSt (cFuncall (cFuncName f) (L.map (cVar . bufName) args)) ""

timeOperationWithDimVals dimVals bufsAndIVars func fCall =
    let argInfo = L.takeWhile (\b -> isCPtr $ bufType b) bufsAndIVars
        indInfo = L.dropWhile (\b -> isCPtr $ bufType b) bufsAndIVars
        indDecls = bufDecls indInfo
        indNames = L.map bufName indInfo
        indVars = L.map cVar indNames
        indInits = L.map (\indVar -> cExprSt (cAssign indVar (cIntLit 79)) "") indVars
        setupCode = setupCodeGS dimVals bufsAndIVars
        tearDownCode = tearDownCodeGS bufsAndIVars
        allDecls = bufDecls bufsAndIVars in
  timeFunctionWithSetup "time_w_setup" func fCall allDecls setupCode tearDownCode

setupCodeGS dimVals bufsAndIVars =
  initIVars ++
  initBuffers ++
  setBuffersToRandValues
  where
    initIVars = L.map (\(s, v) -> cExprSt (cAssign (cVar s) (cIntLit v)) "") dimVals
    argInfo = L.takeWhile (\b -> isCPtr $ bufType b) bufsAndIVars
    initBuffers = L.map initializeBuffer argInfo
    setBuffersToRandValues = L.map setArgToRandValuesCode argInfo

tearDownCodeGS bufsAndIVars = freeBuffers
  where
    argInfo = L.takeWhile (\b -> isCPtr $ bufType b) bufsAndIVars
    freeBuffers = L.map freeBuffer argInfo
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
