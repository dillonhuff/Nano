module CBackEnd.SanityCheckHarness(sanityCheckHarness,
                                   scSuccessString,
                                   scFailString) where

import Data.List as L

import CBackEnd.CodeGeneration
import CBackEnd.Syntax

sanityCheckHarness :: String -> String -> [BufferInfo] -> CTopLevelItem String
sanityCheckHarness scOpName testOpName argInfo =
  cFuncDecl cVoid "sanity_check" [(cPtr cFILE, "df")] $ cBlock allDecls scBody 
  where
    allDecls = (bufDecls argInfo) ++ (refBufDecls argInfo) ++ (testBufDecls argInfo) ++ (scResultVarDecls argInfo)
    scBody = sanityCheckBody scOpName testOpName argInfo

sanityCheckBody scOpName testOpName argInfo =
  initAllBuffers ++
  setMainBuffersToRandValues ++
  copyMainBuffersToRefBuffers ++
  copyMainBuffersToTestBuffers ++
  callRef ++
  callMain ++
  setSCResultVars ++
  compareSCResultVars
  where
    scResultVars = L.map snd $ scResultVarDecls argInfo
    refBufs = L.map refBufInfo argInfo
    testBufs = L.map testBufInfo argInfo
    allBuffersInfo = argInfo ++ refBufs ++ testBufs
    initAllBuffers = L.map initializeBuffer allBuffersInfo
    setMainBuffersToRandValues = L.map setArgToRandValuesCode argInfo
    copyMainBuffersToRefBuffers = L.zipWith copyBufferCode refBufs argInfo
    copyMainBuffersToTestBuffers = L.zipWith copyBufferCode testBufs argInfo
    callRef = [cExprSt (cFuncall scOpName $ L.map (\bufInfo -> cVar $ bufName bufInfo) refBufs) ""]
    callMain = [cExprSt (cFuncall testOpName $ L.map (\bufInfo -> cVar $ bufName bufInfo) testBufs) ""]
    setSCResultVars = L.zipWith3 setSCResultVar scResultVars refBufs testBufs
    compareSCResultVars = [compareVars $ L.map cVar scResultVars]

initializeBuffer bufInfo = cExprSt (cAssign (cVar $ bufName bufInfo) (cFuncall "malloc" [bufSizeExpr bufInfo])) ""

bufDecls argInfo = L.map (\info -> (bufType info, bufName info)) argInfo
refBufDecls argInfo = L.map (\info -> (bufType info, (bufName info) ++ "_ref")) argInfo
testBufDecls argInfo = L.map (\info -> (bufType info, (bufName info) ++ "_test")) argInfo
scResultVarDecls argInfo = L.map (\info -> (cInt, (bufName info) ++ "_sc_result")) argInfo

refBufInfo bufInfo = bufferInfo ((bufName bufInfo) ++ "_ref") (bufType bufInfo) (bufSize bufInfo) (bufScope bufInfo)
testBufInfo bufInfo = bufferInfo ((bufName bufInfo) ++ "_test") (bufType bufInfo) (bufSize bufInfo) (bufScope bufInfo)

setArgToRandValuesCode :: BufferInfo -> CStmt String
setArgToRandValuesCode argInfo =
  let name = bufName argInfo
      tp = bufType argInfo
      sz = bufSize argInfo in
  case getReferencedType tp == cDouble of
    True -> cExprSt (cFuncall "rand_doubles" [sz, cVar name]) ""
    False -> case getReferencedType tp == cFloat of
      True -> cExprSt (cFuncall "rand_floats" [sz, cVar name]) ""
      False -> error $ "Unrecognized type in setArgToRandValuesCode " ++ show tp

copyBufferCode :: BufferInfo -> BufferInfo -> CStmt String
copyBufferCode destBuf srcBuf =
  cExprSt (cFuncall "memcpy" [cVar $ bufName destBuf, cVar $ bufName srcBuf, bufSizeExpr destBuf]) ""

bufSizeExpr bufInfo = cMul (cSizeOf (getReferencedType $ bufType bufInfo)) (bufSize bufInfo)      

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
