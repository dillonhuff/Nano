module CBackEnd.TimingHarness(timingHarness) where

import Data.List as L

import CBackEnd.Syntax
import CBackEnd.Utils

timingHarness :: String -> [BufferInfo] -> CTopLevelItem String
timingHarness funcName bufInfo =
  cFuncDecl cVoid "time_impl" [(cPtr cFILE, "df")] $ cBlock allDecls bodyStmts
  where
    allDecls = (bufDecls bufInfo) ++ timingVarDecls
    bodyStmts = timingBody funcName bufInfo

timingVarDecls = [(cULongLong, "start"),
                  (cULongLong, "end"),
                  (cULongLong, "total_cycles"),
                  (cULongLong, "lvar"),
                  (cULongLong, "num_runs"),
                  (cDouble, "avg_cycles_per_run")]

timingBody funcName bufInfo =
  bufferAllocation ++
  setBuffersToRandomValues ++
  (runCountLoop funcName bufInfo) ++
  (timedLoop funcName bufInfo) ++
  avgCyclesPerRunComputation ++
  writeTimingResults ++
  bufferFreeing
  where
    bufferAllocation = L.map initializeBuffer bufInfo
    setBuffersToRandomValues = L.map setArgToRandValuesCode bufInfo
    bufferFreeing = L.map freeBuffer bufInfo

runCountLoop funcName bufInfo =
  [cExprSt (cAssign (cVar "num_runs") (cIntLit 0)) "",
   cExprSt (cAssign (cVar "total_cycles") (cIntLit 0)) ""] ++
  (runCountBody funcName bufInfo)

runCountBody funcName bufInfo =
  [cWhile (cLEQ (cVar "total_cycles") (cIntLit 100000000)) (runCountWhileBody funcName bufInfo) ""]

runCountWhileBody funcName bufInfo =
  cBlock [] body
  where
    addToTotal = cExprSt (cAssign (cVar "total_cycles") (cAdd (cVar "total_cycles") (cSub (cVar "start") (cVar "end")))) ""
    incRuns = cExprSt (cAssign (cVar "num_runs") (cAdd (cVar "num_runs") (cIntLit 1))) ""
    callFunc = cExprSt (cFuncall funcName $ L.map (\info -> cVar $ bufName info) bufInfo) ""
    body = startRDTSC ++ [callFunc] ++ endRDTSC ++ [addToTotal, incRuns]

timedLoop funcName bufInfo =
  startRDTSC ++
  (timedLoopBody funcName bufInfo) ++
  endRDTSC

timedLoopBody funcName bufInfo =
  [cFor (cAssign (cVar "lvar") (cIntLit 1)) (cLEQ (cVar "lvar") (cVar "num_runs")) (cAssign (cVar "lvar") (cAdd (cVar "lvar") (cIntLit 1))) (cBlock [] $ timeLoopBody funcName bufInfo) ""]

timeLoopBody funcName bufInfo =
  [cExprSt (cFuncall funcName $ L.map (\info -> cVar $ bufName info) bufInfo) ""]

avgCyclesPerRunComputation = [cExprSt (cAssign (cVar "avg_cycles_per_run") avgVal) ""]
  where
    avgVal = cDiv (cSub (cVar "end") (cVar "start")) (cCast (cDouble) (cVar "num_runs"))

writeTimingResults = [cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"%f\\n\"", cVar "avg_cycles_per_run"]) ""]

startRDTSC = [cExprSt (cAssign (cVar "start") (cFuncall "rdtsc" [])) ""]
endRDTSC = [cExprSt (cAssign (cVar "end") (cFuncall "rdtsc" [])) ""]
