module CBackEnd.Utils(initializeBuffer, freeBuffer,
                      bufSizeExpr,
                      bufDecls,
                      setArgToRandValuesCode) where

import Data.List as L

import CBackEnd.Syntax

initializeBuffer bufInfo = cExprSt (cAssign (cVar $ bufName bufInfo) (cFuncall "malloc" [bufSizeExpr bufInfo])) ""

freeBuffer bufInfo = cExprSt (cFuncall "free" [cVar $ bufName bufInfo]) ""

bufSizeExpr bufInfo = cMul (cSizeOf (getReferencedType $ bufType bufInfo)) (bufSize bufInfo)

bufDecls argInfo = L.map (\info -> (bufType info, bufName info)) argInfo

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
