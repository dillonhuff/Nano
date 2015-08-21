module CBackEnd.CodeGeneration.AVX.CodeGenState(CodeGenState,
                                                codeGenState,
                                                codeGenVars,
                                                codeGenPrefix, codeGenIndex,
                                                freshTempVar,
                                                registerGroupVars) where

import Control.Monad.State
import Data.List as L
import Data.Map as M

import CBackEnd.Syntax
import Core.IndexExpression
import Core.Matrix
import Core.MemLocation

data CodeGenState
  = CodeGenState {
    codeGenVars :: [(CType, String)],
    codeGenPrefix :: String,
    codeGenIndex :: Int,
    codeGenRegGroups :: Map Matrix [CExpr]
    } deriving (Eq, Ord, Show)

codeGenState prefix = CodeGenState [] prefix 0 M.empty

findRegisterGroup m cgs =
  M.lookup m $ codeGenRegGroups cgs

freshTemp :: CodeGenState -> (String, CodeGenState)
freshTemp (CodeGenState vs pre i rm) =
  (pre ++ show i, CodeGenState ((cM256dReg, pre ++ show i):vs) pre (i+1) rm)

addRegs :: [String] -> State CodeGenState ()
addRegs newRegNames = do
  (CodeGenState vs p i rm) <- get
  put $ CodeGenState (vs ++ (L.map (\n -> (cM256dReg, n)) newRegNames)) p i rm

addRegGroup :: Matrix -> [String] -> State CodeGenState()
addRegGroup m newRegNames = do
  addRegs newRegNames
  (CodeGenState vs p i rm) <- get
  put $ CodeGenState vs p i (M.insert m (L.map cVar newRegNames) rm)

freshTempVar :: State CodeGenState CExpr
freshTempVar = do
  cgs <- get
  let (freshName, newCGS) = freshTemp cgs in
    do
      put newCGS
      return $ cVar freshName

registerGroupVars :: Matrix -> State CodeGenState [CExpr]
registerGroupVars m = do
  cgs <- get
  case findRegisterGroup m cgs of
   Just r -> return r
   Nothing -> createRegisterGroup m

createRegisterGroup m =
  let orient = matRegGroupFormat m
      nRegs = numRegsInGroup m
      regPrefixes = L.map (\n -> n ++ show orient) $ L.replicate nRegs $ bufferName m
      regNames = L.zipWith (++) regPrefixes $ L.map show $ [0..nRegs - 1] in
   do
     addRegGroup m regNames
     return $ L.map cVar regNames

numRegsInGroup m =
  case matRegGroupFormat m of
   ByRow -> constVal $ numRows m
   ByCol -> constVal $ numCols m
