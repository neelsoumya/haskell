type Expr = [[Int]]

e1 = [[-1, 2, 4], [-2, -3]]

{- function to evaluate a literal given a list of variable assignments. If no match found in list of variable assignments, then it substitutes True for it -}
evalLiteral :: Int -> [(Int,Bool)] -> Bool
evalLiteral c [] = True
evalLiteral c ((v,boolval):l)
		| c == v  =  boolval
		| c == - v  =  not boolval
		| otherwise = evalLiteral c l


{- function to evaluate a clause given a partial list of variable assignments -}
evalClausePartial :: [(Int,Bool)] -> [Int] -> Bool
evalClausePartial vs es = or[evalLiteral e vs | e <- es]

{- function to evaluate whole formula given a partial list of variable assignments -}
evalFormulaPartial :: [(Int,Bool)] -> Expr -> Bool
evalFormulaPartial vs es = and(map (evalClausePartial vs) es)

{- function to do backtracking to find satisfiable assignments (if possible) to formula -}
backTrack :: [Int] -> Expr -> [(Int,Bool)] -> Bool
backTrack [] [] _ = False
backTrack [] _ [] = False
backTrack [] _ _ = True
backTrack (v:vs) expr currVarAss = if evalFormulaPartial ((v,True):currVarAss) expr 
					then backTrack vs expr ((v,True): currVarAss) 
					else if evalFormulaPartial ((v,False):currVarAss) expr
						then backTrack vs expr ((v,False):currVarAss)
						else False

{- function to generate the satisfying assignments (if any) -}
backTrack' :: [Int] -> Expr -> [(Int,Bool)] -> [(Int,Bool)]
backTrack' [] [] _ = []
backTrack' [] _ [] = [] 
backTrack' [] _ currVarAss = currVarAss
backTrack' (v:vs) expr currVarAss = if evalFormulaPartial ((v,True):currVarAss) expr 
                                        then backTrack' vs expr ((v,True): currVarAss)
                                        else if evalFormulaPartial ((v,False):currVarAss) expr
                                                then backTrack' vs expr ((v,False):currVarAss)
                                                else []


