module SanityCheckHarness(sanityCheckHarness) where

import Data.List as L

import CBackEnd
import CGen

sanityCheckHarness :: String -> String -> [ArgumentInfo] -> CTopLevelItem String
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
    callRef = [cExprSt (cFuncall scOpName $ L.map (\bufInfo -> cVar $ argName bufInfo) refBufs) ""]
    callMain = [cExprSt (cFuncall testOpName $ L.map (\bufInfo -> cVar $ argName bufInfo) testBufs) ""]
    setSCResultVars = L.zipWith3 setSCResultVar scResultVars refBufs testBufs
    compareSCResultVars = [compareVars $ L.map cVar scResultVars]

initializeBuffer bufInfo = cExprSt (cAssign (cVar $ argName bufInfo) (cFuncall "malloc" [bufSizeExpr bufInfo])) ""



bufDecls argInfo = L.map (\info -> (argType info, argName info)) argInfo
refBufDecls argInfo = L.map (\info -> (argType info, (argName info) ++ "_ref")) argInfo
testBufDecls argInfo = L.map (\info -> (argType info, (argName info) ++ "_test")) argInfo
scResultVarDecls argInfo = L.map (\info -> (cInt, (argName info) ++ "_sc_result")) argInfo

refBufInfo bufInfo = argumentInfo ((argName bufInfo) ++ "_ref") (argType bufInfo) (argSize bufInfo)
testBufInfo bufInfo = argumentInfo ((argName bufInfo) ++ "_test") (argType bufInfo) (argSize bufInfo)

setArgToRandValuesCode :: ArgumentInfo -> CStmt String
setArgToRandValuesCode argInfo =
  let name = argName argInfo
      tp = argType argInfo
      sz = argSize argInfo in
  case getReferencedType tp == cDouble of
    True -> cExprSt (cFuncall "rand_doubles" [sz, cVar name]) ""
    False -> case getReferencedType tp == cFloat of
      True -> cExprSt (cFuncall "rand_floats" [sz, cVar name]) ""
      False -> error $ "Unrecognized type in setArgToRandValuesCode " ++ show tp

copyBufferCode :: ArgumentInfo -> ArgumentInfo -> CStmt String
copyBufferCode destBuf srcBuf =
  cExprSt (cFuncall "memcpy" [cVar $ argName destBuf, cVar $ argName srcBuf, bufSizeExpr destBuf]) ""

bufSizeExpr bufInfo = cMul (cSizeOf (getReferencedType $ argType bufInfo)) (argSize bufInfo)      

setSCResultVar varName refBuf testBuf =
  let argList = [argSize testBuf, cVar $ argName refBuf, cVar $ argName testBuf] in
  case getReferencedType (argType refBuf) == cDouble of
    True -> cExprSt (cAssign (cVar varName) (cFuncall "test_buffer_diff" argList)) ""
    False -> cExprSt (cAssign (cVar varName) (cFuncall "test_buffer_diff_float" argList)) ""

compareVars varList =
  cIfThenElse (orExprs varList) (cBlock [] [writeSCFail]) (cBlock [] [writeSCSuccess]) ""

writeSCFail =
  cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"false\\n\""]) ""

writeSCSuccess =
  cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"true\\n\""]) ""

orExprs [e] = e
orExprs (e2:rest) = cOr e2 $ orExprs rest
