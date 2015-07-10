module CBackEnd.SanityCheckHarness(sanityCheckHarness,
                                   sanityCheckHarnessS,
                                   scSuccessString,
                                   scFailString) where

import Data.List as L

import CBackEnd.CodeGeneration.Core
import CBackEnd.Syntax
import CBackEnd.Utils

sanityCheckHarness :: String -> String -> [BufferInfo] -> CTopLevelItem String
sanityCheckHarness scOpName testOpName argInfo =
  sanityCheckHarnessS [] [] scOpCall testOpCall argInfo
  where
    scOpCall = \refBufs -> [cExprSt (cFuncall scOpName $ L.map (\bufInfo -> cVar $ bufName bufInfo) refBufs) ""]
    testOpCall = \testBufs -> [cExprSt (cFuncall testOpName $ L.map (\bufInfo -> cVar $ bufName bufInfo) testBufs) ""]

sanityCheckHarnessS :: [(CType, String)] ->
                       [CStmt String] ->
                       ([BufferInfo] -> [CStmt String]) ->
                       ([BufferInfo] -> [CStmt String]) ->
                       [BufferInfo] ->
                       CTopLevelItem String
sanityCheckHarnessS additionalDecls setupCode scOpFuncall testOpFuncall argInfo =
  cFuncDecl cVoid "sanity_check" [(cPtr cFILE, "df")] $ cBlock allDecls scBody
  where
    bufferDecls = (bufDecls argInfo) ++ (refBufDecls argInfo) ++ (testBufDecls argInfo) ++ (scResultVarDecls argInfo)
    allDecls = additionalDecls ++ bufferDecls
    scBody = sanityCheckBody setupCode scOpFuncall testOpFuncall argInfo

sanityCheckBody setupCode scOpFuncall testOpFuncall argInfo =
  setupCode ++
  initAllBuffers ++
  setMainBuffersToRandValues ++
  copyMainBuffersToRefBuffers ++
  copyMainBuffersToTestBuffers ++
  (scOpFuncall refBufs) ++
  (testOpFuncall testBufs) ++
  setSCResultVars ++
  compareSCResultVars ++
  freeAllBuffers
  where
    scResultVars = L.map snd $ scResultVarDecls argInfo
    refBufs = L.map refBufInfo argInfo
    testBufs = L.map testBufInfo argInfo
    allBuffersInfo = argInfo ++ refBufs ++ testBufs
    initAllBuffers = L.map initializeBuffer allBuffersInfo
    setMainBuffersToRandValues = L.map setArgToRandValuesCode argInfo
    copyMainBuffersToRefBuffers = L.zipWith copyBufferCode refBufs argInfo
    copyMainBuffersToTestBuffers = L.zipWith copyBufferCode testBufs argInfo
    setSCResultVars = L.zipWith3 setSCResultVar scResultVars refBufs testBufs
    compareSCResultVars = [compareVars $ L.map cVar scResultVars]
    freeAllBuffers = L.map freeBuffer argInfo

refBufDecls argInfo = L.map (\info -> (bufType info, (bufName info) ++ "_ref")) argInfo
testBufDecls argInfo = L.map (\info -> (bufType info, (bufName info) ++ "_test")) argInfo
scResultVarDecls argInfo = L.map (\info -> (cInt, (bufName info) ++ "_sc_result")) argInfo

refBufInfo bufInfo = bufferInfo ((bufName bufInfo) ++ "_ref") (bufType bufInfo) (bufSize bufInfo) (bufScope bufInfo)
testBufInfo bufInfo = bufferInfo ((bufName bufInfo) ++ "_test") (bufType bufInfo) (bufSize bufInfo) (bufScope bufInfo)

copyBufferCode :: BufferInfo -> BufferInfo -> CStmt String
copyBufferCode destBuf srcBuf =
  cExprSt (cFuncall "memcpy" [cVar $ bufName destBuf, cVar $ bufName srcBuf, bufSizeExpr destBuf]) ""

setSCResultVar varName refBuf testBuf =
  let argList = [bufSize testBuf, cVar $ bufName refBuf, cVar $ bufName testBuf] in
  case getReferencedType (bufType refBuf) == cDouble of
    True -> cExprSt (cAssign (cVar varName) (cFuncall "test_buffer_diff" argList)) ""
    False -> cExprSt (cAssign (cVar varName) (cFuncall "test_buffer_diff_float" argList)) ""

compareVars varList =
  cIfThenElse (orExprs varList) (cBlock [] [writeSCFail]) (cBlock [] [writeSCSuccess]) ""

writeSCFail =
  cExprSt (cFuncall "fprintf" [cVar "df", cVar scFailString]) ""

writeSCSuccess =
  cExprSt (cFuncall "fprintf" [cVar "df", cVar scSuccessString]) ""

scFailString = "\"false\\n\""
scSuccessString = "\"true\\n\""

orExprs [e] = e
orExprs (e2:rest) = cOr e2 $ orExprs rest
