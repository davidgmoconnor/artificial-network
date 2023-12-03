module Core.Solver 
  ( solve )
where

import Protolude hiding ( sum )
import Core.Network ( Network, Line (..), InsurerID (..), Percentage (..), Expression (..), sum )

type Candidate = [Line]

solve :: Int -> Network -> [Int]
solve p n = do
  let cs = makeCandidates n
  let solutions = filter (isValid p) cs
  let result = head $ sortOn length solutions
  case result of 
    Nothing -> []
    Just r  -> map (unInsurerID . insurer) r

makeCandidates :: Network -> [Candidate]
makeCandidates [] = [[]]
makeCandidates (x:xs) = map (x:) (makeCandidates xs) ++ makeCandidates xs

isValid :: Int -> Candidate -> Bool
isValid p c = do
  let totalMatch = unPercentage (foldr sum (Percentage 0) (map percentage c )) == 100
  if not totalMatch 
    then False
    else foldr (&&) True $ map (checkLineExpressions p c) (map expressions c)

checkLineExpressions :: Int -> Candidate -> [Expression] -> Bool
checkLineExpressions p c es = foldr (&&) True $ map (checkExpression p c) es

checkExpression :: Int -> Candidate -> Expression -> Bool
checkExpression p _ (LessThan x) = p < x
checkExpression p _ (MoreThan x) = p > x
checkExpression p _ (EqualTo x)  = p == x
checkExpression _ c (Insurer i)  = case filteredInsurer of 
                                      [] -> False
                                      _  -> True
                                   where filteredInsurer = (filter ((==i) . unInsurerID . insurer) c)