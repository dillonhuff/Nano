module CBackEnd.CodeGeneration.AVX.CodeGenState(CodeGenState,
                                                codeGenState,
                                                codeGenVars, codeGenPrefix, codeGenIndex,
                                                freshTempVar) where

import Control.Monad.State

import CBackEnd.Syntax

data CodeGenState
  = CodeGenState {
    codeGenVars :: [(CType, String)],
    codeGenPrefix :: String,
    codeGenIndex :: Int
    } deriving (Eq, Ord, Show)

codeGenState prefix = CodeGenState [] prefix 0

freshTemp :: CodeGenState -> (String, CodeGenState)
freshTemp (CodeGenState vs pre i) =
  (pre ++ show i, CodeGenState ((cM256dReg, pre ++ show i):vs) pre (i+1))

freshTempVar :: State CodeGenState CExpr
freshTempVar = do
  cgs <- get
  let (freshName, newCGS) = freshTemp cgs in
    do
      put newCGS
      return $ cVar freshName
