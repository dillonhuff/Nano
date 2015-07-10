module CBackEnd.TimingHarness(timingHarness,
                              timingHarnessSetup) where

import Data.List as L

import CBackEnd.Syntax
import CBackEnd.Utils

timingHarness :: String -> [BufferInfo] -> CTopLevelItem String
timingHarness funcName bufInfo =
  timingHarnessSetup [ft] bufDecs allocAndSetRand freeBufs
  where
    ft = cExprSt (cFuncall funcName $ L.map (\info -> cVar $ bufName info) bufInfo) ""
    allocAndSetRand = (L.map initializeBuffer bufInfo) ++ (L.map setArgToRandValuesCode bufInfo)
    freeBufs = L.map freeBuffer bufInfo
    bufDecs = bufDecls bufInfo

timingHarnessSetup :: [CStmt String] -> [(CType, String)] -> [CStmt String] -> [CStmt String] -> CTopLevelItem String
timingHarnessSetup codeToTime varDecls setupCode tearDownCode =
  cFuncDecl cVoid "time_impl" [(cPtr cFILE, "df")] $ cBlock allDecls bodyStmts
  where
    allDecls = varDecls ++ timingVarDecls
    bodyStmts = timingBody codeToTime setupCode tearDownCode

timingVarDecls = [(cULongLong, "start"),
                  (cULongLong, "end"),
                  (cULongLong, "total_cycles"),
                  (cULongLong, "lvar"),
                  (cULongLong, "num_runs"),
                  (cDouble, "avg_cycles_per_run")]

timingBody codeToTime setupCode tearDownCode =
  setupCode ++
  (runCountLoop codeToTime) ++
  (timedLoop codeToTime) ++
  avgCyclesPerRunComputation ++
  writeTimingResults ++
  tearDownCode
  
runCountLoop codeToTime =
  [cExprSt (cAssign (cVar "num_runs") (cIntLit 0)) "",
   cExprSt (cAssign (cVar "total_cycles") (cIntLit 0)) ""] ++
  (runCountBody codeToTime)

runCountBody codeToTime =
  [cWhile (cLEQ (cVar "total_cycles") (cIntLit 100000000)) (runCountWhileBody codeToTime) ""]

runCountWhileBody codeToTime =
  cBlock [] body
  where
    addToTotal = cExprSt (cAssign (cVar "total_cycles") (cAdd (cVar "total_cycles") (cSub (cVar "end") (cVar "start")))) ""
    incRuns = cExprSt (cAssign (cVar "num_runs") (cAdd (cVar "num_runs") (cIntLit 1))) ""
    body = startRDTSC ++ codeToTime ++ endRDTSC ++ [addToTotal, incRuns]

timedLoop codeToTime =
  startRDTSC ++
  (timedLoopBody codeToTime) ++
  endRDTSC

timedLoopBody codeToTime =
  [cFor (cAssign (cVar "lvar") (cIntLit 1)) (cLEQ (cVar "lvar") (cVar "num_runs")) (cAssign (cVar "lvar") (cAdd (cVar "lvar") (cIntLit 1))) (cBlock [] codeToTime) ""]

avgCyclesPerRunComputation = [cExprSt (cAssign (cVar "avg_cycles_per_run") avgVal) ""]
  where
    avgVal = cDiv (cSub (cVar "end") (cVar "start")) (cCast (cDouble) (cVar "num_runs"))

writeTimingResults = [cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"%f\\n\"", cVar "avg_cycles_per_run"]) ""]

startRDTSC = [cExprSt (cAssign (cVar "start") (cFuncall "rdtsc" [])) ""]
endRDTSC = [cExprSt (cAssign (cVar "end") (cFuncall "rdtsc" [])) ""]
