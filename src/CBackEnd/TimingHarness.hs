module CBackEnd.TimingHarness(timingHarness) where

import Data.List as L

import CBackEnd.Syntax
import CBackEnd.Utils

timingHarness :: String -> [BufferInfo] -> CTopLevelItem String
timingHarness funcName bufInfo =
  timingHarnessS ft bufDecs allocAndSetRand freeBufs
  where
    ft = cExprSt (cFuncall funcName $ L.map (\info -> cVar $ bufName info) bufInfo) ""
    allocAndSetRand = (L.map initializeBuffer bufInfo) ++ (L.map setArgToRandValuesCode bufInfo)
    freeBufs = L.map freeBuffer bufInfo
    bufDecs = bufDecls bufInfo

timingHarnessS :: CStmt String -> [(CType, String)] -> [CStmt String] -> [CStmt String] -> CTopLevelItem String
timingHarnessS funcallToTime varDecls setupCode tearDownCode =
  cFuncDecl cVoid "time_impl" [(cPtr cFILE, "df")] $ cBlock allDecls bodyStmts
  where
    allDecls = varDecls ++ timingVarDecls
    bodyStmts = timingBody funcallToTime setupCode tearDownCode

timingVarDecls = [(cULongLong, "start"),
                  (cULongLong, "end"),
                  (cULongLong, "total_cycles"),
                  (cULongLong, "lvar"),
                  (cULongLong, "num_runs"),
                  (cDouble, "avg_cycles_per_run")]

timingBody funcallToTime setupCode tearDownCode =
  setupCode ++
  (runCountLoop funcallToTime) ++
  (timedLoop funcallToTime) ++
  avgCyclesPerRunComputation ++
  writeTimingResults ++
  tearDownCode
  
runCountLoop funcallToTime =
  [cExprSt (cAssign (cVar "num_runs") (cIntLit 0)) "",
   cExprSt (cAssign (cVar "total_cycles") (cIntLit 0)) ""] ++
  (runCountBody funcallToTime)

runCountBody funcallToTime =
  [cWhile (cLEQ (cVar "total_cycles") (cIntLit 100000000)) (runCountWhileBody funcallToTime) ""]

runCountWhileBody funcallToTime =
  cBlock [] body
  where
    addToTotal = cExprSt (cAssign (cVar "total_cycles") (cAdd (cVar "total_cycles") (cSub (cVar "start") (cVar "end")))) ""
    incRuns = cExprSt (cAssign (cVar "num_runs") (cAdd (cVar "num_runs") (cIntLit 1))) ""
    callFunc = funcallToTime
    body = startRDTSC ++ [callFunc] ++ endRDTSC ++ [addToTotal, incRuns]

timedLoop funcallToTime =
  startRDTSC ++
  (timedLoopBody funcallToTime) ++
  endRDTSC

timedLoopBody funcallToTime =
  [cFor (cAssign (cVar "lvar") (cIntLit 1)) (cLEQ (cVar "lvar") (cVar "num_runs")) (cAssign (cVar "lvar") (cAdd (cVar "lvar") (cIntLit 1))) (cBlock [] [funcallToTime]) ""]

avgCyclesPerRunComputation = [cExprSt (cAssign (cVar "avg_cycles_per_run") avgVal) ""]
  where
    avgVal = cDiv (cSub (cVar "end") (cVar "start")) (cCast (cDouble) (cVar "num_runs"))

writeTimingResults = [cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"%f\\n\"", cVar "avg_cycles_per_run"]) ""]

startRDTSC = [cExprSt (cAssign (cVar "start") (cFuncall "rdtsc" [])) ""]
endRDTSC = [cExprSt (cAssign (cVar "end") (cFuncall "rdtsc" [])) ""]
