module Main where


data Term v f = 
		Fun f [Term v f]
		| Var v
		deriving (Show)
type Equation v f = (Term v f, Term v f)

type Binding v f = (v, Term v f)

type Substitution v f = [Binding v f]

data EquationOutcome = HaltWithFailure | HaltWithCycle | NoMatch | Success
			deriving (Show)

unify :: (Eq v, Eq f) => [Equation v f] -> (EquationOutcome, [Equation v f])
unify [] = (Success, [])
unify ((Fun x l1, Fun y l2):l)
			| x == y    = unify ((zip l1 l2) ++ l)
			| otherwise = (HaltWithFailure, []) 	
unify ((Var x,t):l) = if not (occurscheck (Var x) t) then (fst r, [(Var x,t)] ++ (snd r)) else (HaltWithFailure, []) 
					where r = unify l
unify ((t,Var x):l) = unify ((Var x,t):l)
--unify _ = (NoMatch, [])
{-
unify ((t1,t2):l)
		| t1 == t2 = unify(l)
		| otherwise = (HaltWithFailure,[])
-}


occurscheck :: (Eq v, Eq f) => Term v f -> Term v f -> Bool
--occurscheck (Var x) [] = False
occurscheck (Var x) (Var y)
			| x == y    = True
			| otherwise = False	
occurscheck (Var x) (Fun y l) = or (map (occurscheck (Var x)) l)


s1 = Fun "f" [Fun "g" [Fun "a" [], Var "X"], Fun "h" [Fun "f" [Var "Y", Var "Z"]]]
s2 = Fun "g" [Var "Y", Fun "h" [Fun "f" [Var "Z", Var "U"]]]
t1 = Fun "f" [Var "U", Fun "h" [Fun "f" [Var "X", Var "X"]]]
t2 = Fun "g" [Fun "f" [Fun "h" [Var "X"], Fun "a" []],Fun "h" [Fun "f" [Fun "a" [], Fun "b" []]]]
problem = [(s1, t1), (s2, t2)]
problem2 = [(s1,t1)]

main :: IO ()
main =
    do
      print (unify problem2)	

