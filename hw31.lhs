\begin{code}
{- since I ran out of time, I just wrote
down the base case for the opening move
for Red. It should move into d1 -}

type Board = [[Maybe Player]]

data Player = PRed | PGreen
		deriving (Eq, Show)

heuristicStrategyForRed :: Board -> Board
{- opening move from empty board, move to middle position (d1)-}
heuristicStrategyForRed [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]] = 
[[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Just PRed],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

main :: IO ()
main =    
    do
	print (heuristicStrategyForRed 
([[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]))


{- Output

*Main> heuristicStrategyForRed 
([[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]])
[[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Just PRed],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

-}

	
\end{code}
