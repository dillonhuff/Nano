module CBackEnd.CodeGeneration.AVX.CodeGenState(CodeGenState,
                                                codeGenState,
                                                codeGenVars, codeGenPrefix, codeGenIndex) where

import CBackEnd.Syntax

data CodeGenState
  = CodeGenState {
    codeGenVars :: [(CType, String)],
    codeGenPrefix :: String,
    codeGenIndex :: Int
    } deriving (Eq, Ord, Show)

codeGenState prefix = CodeGenState [] prefix 0
