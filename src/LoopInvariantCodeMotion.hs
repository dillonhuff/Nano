module LoopInvariantCodeMotion(pullCodeOutOfLoops) where

import Data.List as L

import IndexExpression
import Matrix
import Statement

pullCodeOutOfLoops stmts =
  expandStatementsBU pullCodeOutOfLoop stmts

pullCodeOutOfLoop :: Statement -> [Statement]
pullCodeOutOfLoop stmt =
  case isLoop stmt of
    True -> pullLoads stmt
    False -> [stmt]

pullLoads stmt =
  let i = iVar $ loopInductionVariable stmt
      b = loopBody stmt
      initInvOps = initialInvOps i b
      allLoopInvOperands = loopInvariantOperands i b initInvOps
      (invStmts, bodyStmts) = L.foldr (partitionBody allLoopInvOperands) ([], []) b in
--  error $ "Inv ops: " ++ show initInvOps
--  error $ "Inv statments: " ++ show invStmts
  case bodyStmts of
    [] -> error $ "Entire body is invariant: " ++ show invStmts ++ "\n" ++ show stmt
    _ -> invStmts ++ [loop (loopInductionVariable stmt) (loopStart stmt) (loopInc stmt) (loopEnd stmt) bodyStmts]

initialInvOps i b =
  let allOpsWritten = allOperandsWritten b
      allOps = L.concatMap (collectValuesFromStmt allOperands) b
      allWrittenMats = L.map underlyingMatrix allOpsWritten
      initialIOps = L.nub $ L.filter (\op -> not (L.elem (underlyingMatrix op) allWrittenMats || partitionedBy i op)) allOps in
  initialIOps
  
partitionBody :: [Matrix] -> Statement -> ([Statement], [Statement]) -> ([Statement], [Statement])
partitionBody loopInvOps stmt (loopInv, body) =
  case (not $ isLoop stmt) && (L.all (\m -> L.elem m loopInvOps) $ allOperands stmt) of
    True -> (stmt:loopInv, body)
    False -> (loopInv, stmt:body)

loopInvariantOperands i stmts current =
  let newInvOps = nextLoopInvOps i stmts current in
  case newInvOps == current of
    True -> current
    False -> loopInvariantOperands i stmts newInvOps

nextLoopInvOps i stmts current =
  let allOps = L.concatMap (collectValuesFromStmt allOperands) stmts
      possibleNewOperands = L.filter (\op -> not $ L.elem op current) allOps in
  L.foldr (addIfInvariant i stmts) current possibleNewOperands

addIfInvariant i stmts operand knownInvOps =
  let allWriteLocs = L.filter (\stmt -> (not $ isLoop stmt) && operandWritten stmt == operand) stmts in
  case (not $ partitionedBy i operand) && L.all (\stmt -> (L.all (\m -> L.elem m knownInvOps) $ operandsRead stmt)) allWriteLocs of
    True -> operand:knownInvOps {-error $ "Adding\n" ++ show operand ++ "\nto\n" ++ show knownInvOps ++ "\niVar is " ++ show i ++
            "Does iVar partition new operand ? " ++ show (partitionedBy i operand) --operand:knownInvOps-}
    False -> knownInvOps

allOperandsWritten stmts =
  L.nub $ L.concatMap (collectValuesFromStmt (\stmt -> if isLoop stmt then [] else [operandWritten stmt])) stmts
