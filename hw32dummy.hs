type Expr = [[Int]]

eval :: (Int -> Bool) -> Expr -> Bool
eval f = and . map (or . (map f))

{-
eval' expr l = eval f expr
		where f x = evalLiteral x l 
-}

g expr l = eval (flip evalLiteral l) expr

{- function to evaluate a literal given a list of
 variable assignments. If no match found in list 
of variable assignments, then it substitutes True
 for it -}
evalLiteral :: Int -> [(Int,Bool)] -> Bool
evalLiteral c [] = True
evalLiteral c ((v,boolval):l)
                | c == v  =  boolval
                | c == - v  =  not boolval
                | otherwise = evalLiteral c l


{- function to generate all the Boolean combinations-}
boolComb :: Int -> [[Bool]]
boolComb 0 = [[]]
boolComb (n + 1) = map (False:) bss ++ map (True:) bss
			where bss = boolComb n 

{- function to remove duplicates -}
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:removeDuplicates (filter (/= x) xs)

{- function that given the expression generates
all the combinations -}
exhaustBool :: Expr -> [[(Int,Bool)]]
exhaustBool expr = map (zip vs) (boolComb (length vs))
			where vs = map abs (removeDuplicates (concat expr))


{- function to test satisfiability -}
satisfiable :: Expr -> Bool
satisfiable expr = or[ eval (flip evalLiteral l) expr | l <- ls ]
			where ls = exhaustBool expr

test2 :: String -> IO Expr
test2 sourceFile = do y <- readFile sourceFile
		      return ([[-1,2,3],[3,4]])


test :: String -> IO Expr
test sourcefile = return ([[-1,2,3],[3,4]])

test1 sourcefile = readFile sourcefile

{- function to read a line and make a clause out of it -}
createClause [] = ([],[])
createClause (x:y:xs)
		| x == '\n' = ([],(y:xs))
		| x == ' ' = (fst (createClause (y:xs)), snd (createClause (y:xs)))
		| x == '-' = ( (negate ((fromEnum y) - 48)) :(fst (createClause xs)), snd (createClause xs))
		| otherwise = ( ((fromEnum x) - 48):(fst (createClause (y:xs))), snd (createClause (y:xs)))
createClause (x:xs)
                | x == '\n' = ([],xs)
                | x == ' ' = (fst (createClause xs), snd (createClause xs))
                | otherwise = ( ((fromEnum x) - 48):(fst (createClause xs)), snd (createClause xs))

{- function to take a string and make a formula 
out of it -}
createFormula [] = []
createFormula (x:xs)
		| x == ' ' = createFormula xs
		| otherwise = [(fst p)] ++ (createFormula (snd p))
				where p = createClause (x:xs)


parse' (x:xs)
		| x == '\n' = createFormula xs
		| otherwise = parse' xs

{- function to take a string and return the 
expression/formula -}
parse (x:xs)
		| x == 'c' = parse xs
		| x == 'p' = parse' xs
		| otherwise = parse xs
