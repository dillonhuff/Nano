module CUtils(orExprs, setSCResVar, indexAssignSt, mainFunc,
              setArgToRandValuesCode) where

import Data.Map as M

import CGen

orExprs [e] = e
orExprs (e2:rest) = cOr e2 $ orExprs rest

setSCResVar :: a -> String -> CType -> CExpr -> CStmt a
setSCResVar dummyAnn n tp sizeExpr =
  case tp == cDouble of
    True -> cExprSt (cAssign (cVar (n ++ "_sc_res")) (cFuncall "test_buffer_diff_double" [cVar (n ++ "_ref"), cVar (n ++ "_test"), sizeExpr])) dummyAnn
    False -> case tp == cFloat of
      True -> cExprSt (cAssign (cVar (n ++ "_sc_res")) (cFuncall "test_buffer_diff_float" [cVar (n ++ "_ref"), cVar (n ++ "_test"), sizeExpr])) dummyAnn
      False -> error $ "Unrecognized type " ++ show tp ++ " in setSCResVar"

indexAssignSt :: a -> String -> Map String Int -> CStmt a
indexAssignSt dummyAnn varName indexVals =
  case M.lookup varName indexVals of
    Just val -> cExprSt (cAssign (cVar varName) (cIntLit val)) dummyAnn
    Nothing -> error $ varName ++ " does not exist in index value list " ++ show indexVals

mainFunc :: String -> CTopLevelItem String
mainFunc dataFileName =
  cFuncDecl cInt "main" [] $
            cBlock [(cPtr cFILE, "data_file")]
                   [cExprSt (cAssign (cVar "data_file") (cFuncall "fopen" [cVar ("\"" ++ dataFileName ++ "\""), cVar "\"w\""])) "",
                    cExprSt (cFuncall "sanity_check_impls" [cVar "data_file"]) "",
                    cExprSt (cFuncall "time_impls" [cVar "data_file"]) "",
                    cExprSt (cFuncall "fclose" [cVar "data_file"]) "",
                    cReturn (cIntLit 0) ""]

setArgToRandValuesCode :: a -> ((String, CType), CExpr) -> CStmt a
setArgToRandValuesCode dummyAnn ((name, tp), sz) =
  case getReferencedType tp == cDouble of
    True -> cExprSt (cFuncall "rand_doubles" [cVar name, sz]) dummyAnn
    False -> case getReferencedType tp == cFloat of
      True -> cExprSt (cFuncall "rand_floats" [cVar name, sz]) dummyAnn
      False -> error $ "Unrecognized type in setArgToRandValuesCode " ++ show tp

