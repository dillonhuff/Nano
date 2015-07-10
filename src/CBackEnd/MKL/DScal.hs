module CBackEnd.MKL.DScal(timeDScal) where

import CBackEnd.Syntax
import CBackEnd.Timing

timeDScal :: Int -> IO String
timeDScal m =
  runTimingCodeForExternalFunction "dscal_test" dscalCall dscalDecls (dscalSetup m) dscalTearDown mklInclude

mklInclude = cInclude "\"mkl.h\""

dscalCall = [cExprSt (cFuncall "cblas_dscal" [cVar "m", cArrAcc (cVar "beta") (cIntLit 0), cVar "x", cIntLit 1]) ""]

dscalDecls = [(cInt, "m"), (cPtr cDouble,  "beta"), (cPtr cDouble, "x")]

dscalSetup m = [cExprSt (cAssign (cVar "m") (cIntLit m)) "",
                cExprSt (cFuncall "mkl_malloc" [betaSize, cVar "beta", cIntLit 32]) "",
                cExprSt (cFuncall "mkl_malloc" [xSize, cVar "x", cIntLit 32]) "",
                cExprSt (cFuncall "rand_doubles" [cIntLit m, cVar "x"]) ""]
  where
    betaSize = cSizeOf cDouble
    xSize = cMul (cSizeOf cDouble) (cIntLit m)

dscalTearDown = [cExprSt (cFuncall "mkl_free" [cVar "beta"]) "",
                 cExprSt (cFuncall "mkl_free" [cVar "x"]) ""]
