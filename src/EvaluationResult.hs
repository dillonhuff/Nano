module EvaluationResult(EvaluationResult,
                                evaluationResult,
                                avgCyclesPerRun,
                                passedSanityCheck) where

import Data.List as L
import Data.Map as M

data EvaluationResult
  = EvaluationResult Double Bool
    deriving (Eq, Ord, Show)

evaluationResult a b = EvaluationResult a b

passedSanityCheck (EvaluationResult _ b) = b
avgCyclesPerRun (EvaluationResult a _) = a

parseTimingResults :: String -> Map String EvaluationResult
parseTimingResults str =
  let (scLines, rtLines) = splitResultsIntoSanityCheckAndTimeSections str
      scResults = parseSCResults scLines
      rtResults = parseRTResults rtLines in
  M.fromList $ mergeResults scResults rtResults

splitResultsIntoSanityCheckAndTimeSections str =
  let strLines = L.lines str
      scLines = L.takeWhile (\l -> l /= scTimingSeparator) strLines
      rtLines = L.dropWhile (\l -> l /= scTimingSeparator) strLines in
  (scLines, rtLines)

scTimingSeparator = "#TIMING_RESULTS"

mergeResults :: [(String, Bool)] -> [(String, Double)] -> [(String, EvaluationResult)]
mergeResults [] _ = []
mergeResults ((n, b):scRest) rtRes =
  case L.lookup n rtRes of
    Just t -> (n, evaluationResult t b) : (mergeResults scRest rtRes)
    Nothing -> error $ "Could not find " ++ n ++ " in " ++ show rtRes

parseRTResults :: [String] -> [(String, Double)]
parseRTResults [] = []
parseRTResults (_:n:avgCyclesPerRun:rest) = (n, read avgCyclesPerRun) : (parseRTResults rest)
parseRTResults other = error $ "parseRTResults: " ++ show other

parseSCResults :: [String] -> [(String, Bool)]
parseSCResults [] = []
parseSCResults (n:passFail:rest) = (n, if passFail == "passed" then True else False):(parseSCResults rest)
parseSCResults other = error $ "parseSCResults failed with " ++ show other
