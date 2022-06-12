
elem'          :: (Eq a) => a -> [a] -> Bool
elem' m []     = False
elem' m (n:ns) = if m == n then True else elem' m ns


setEqual           :: [Integer] -> [Integer] -> Bool
setEqual [] [] = True
setEqual [] x = False
setEqual x [] = False
setEqual (m:ms) (n:ns) = (m == n) && (setEqual ms ns)

setDiff [] x = []
setDiff x [] = x
setDiff (m:ms) (n:ns) = if elem' m (n:ns) then setDiff ms (n:ns) else m:(setDiff ms (n:ns))


setIntersection  :: [Integer] -> [Integer] -> [Integer]
setIntersection [] x = []
setIntersection (m:ms) (n:ns) = if elem' m (n:ns) then m:(setIntersection ms (n:ns)) else setIntersection ms (n:ns)

setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion xs ys = (setDiff xs ys) ++ ys

powerSet        :: [Integer] -> [[Integer]]
powerSet    []  = []
powerSet (n:ns) = (n:ns) : (powerSet ns)


