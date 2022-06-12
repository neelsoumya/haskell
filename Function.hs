add	:: (Int,Int) -> Int
add (a, b) = a + b

zeroton :: (Int) -> [Int]
zeroton n = [0..n] 

add'	:: Int -> (Int -> Int)
add' a b = a + b 

sum1	     :: [Int] -> Int
sum1 (n:ns)  =  n + (sum1 ns)

{-palindrome :: [a] -> Bool
palindrome xs = reverse xs == xs -}

{-abs'	:: (Num a) -> a -> a  -}
abs'	:: Int -> Int
abs' n  = if n >= 0 then n else -n

signum'  :: Int -> Int
signum' n = if n < 0 then -1 else
	      if n == 0 then 0 else 1	

signum'' n | n < 0     = -1
	   | n == 0    = 0
	   | otherwise = 1

and1    :: Bool -> Bool -> Bool
True `and1` True = True
_    `and1` _    = False

{-  and2    :: (Bool,Bool) -> Bool
True and2 True = True
_    and2 _    = False  -}

and3    :: Bool -> Bool -> Bool
and3 True True = True
and3 _ _    = False

head1        :: [a] -> a
head1 (x:xs) = x

head2       :: [a] -> a
head2 (x:_) = x

tail1        :: [a] -> [a]
tail1 (x:xs) = xs 

{-  (\x -> x + x) 5  -}

{-  (\x -> \y -> x*5 + y) 5 3  -}

{-  (\x y -> x*5 + y) 5 3  -}

add4 = (\x -> \y -> x + y) 

const1     :: a -> b -> a
const1 x _ = x

const2     :: a -> b -> b
const2 _ x = x

odd1 n = map f [0..n-1]
	where
		f x = x*2 + 1

{-  odd2   :: (Integer a) => a -> [a]  -}
odd2 n = map (\x -> x*2  +1) [0..n-1]

safetail    :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs  

safetail1   :: [a] -> [a]
safetail1 (x:xs) = if null((x:xs)) then [] else xs

safetail2   :: [a] -> [a]
safetail2 (x:xs) | null([x]) = []
		 | otherwise  = xs

tail2        :: [a] -> [a]
tail2 (x:xs) = xs

safetail3   :: [a] -> [a]
safetail3 xs | null(xs)   = []
	     | otherwise  = tail2 xs

safetail4   :: [a] -> [a]
safetail4 xs | null(xs)   = []
	     | otherwise  = (\(y:ys) -> ys) xs

safetail5   :: [a] -> [a]
safetail5 xs | (\ys -> if length(ys) == 0 then True else False)(xs)   = []
	     | otherwise  = (\(z:zs) -> zs) xs

{-  safetail6   :: [a] -> [a]
safetai6 xs | (\ys -> if (\p)(ys) == 0 then True else False)(xs)   = []
	     | otherwise  = (\(z:zs) -> zs) xs
-}

{-  and4        :: Bool -> Bool -> Bool
and4 x y    = if x == True  -} 

{-  [x^2 | x <- [1..5] ]   -}

{- test       :: [a] -> [a] -> (a,a) 
test xs ys = [(x,y) | x <- xs, y <- ys]  -}

concat1      :: [[a]] -> [a]
concat1  xss = [x | xs <- xss, x <- xs] 

even2        :: [Int] -> [Int]
even2 xs     = [x | x <- xs, x `mod` 2 == 0] 

even1        :: [Int] -> [Int]
even1 xs     = [x | x <- xs, even x]

factors1     :: Int -> [Int]
factors1  n  = [x | x <- [1..n], n `mod` x == 0]

primes1      :: Int -> Bool
primes1 n    = (factors1 n) == [1,n]

{-  (\x -> factors1 x == [1,x])24  -}

primefactors   :: Int -> [Int]
primefactors n = [x | x <- factors1 n, factors1 x == [1,x]] 

{-  zip1         :: [a] -> [b] -> [(a,b)]
zip1  xs ys  = [(x,y) | x <- xs, y <-ys]  -}

pairs1       :: [a] -> [(a,a)]
pairs1   xs  = zip xs (tail xs) 

sorted1       :: (Ord a) => [a] -> Bool
sorted1   xs  = and [x <= y | (x,y) <- pairs1 xs]

{-   lowers1        :: [Char] -> Int
lowers1   ns   = length [n | n <- ns, isLower n]  -}

pyth       :: Int -> [(Int,Int,Int)]
pyth    n  = [(x,y,n) | x <- [1..n], y <- [1..n], x^2 + y^2 == n^2]

setEqual       :: [Integer] -> [Integer] -> Bool
setEqual ms ns = sorted1 ms == sorted1 ns 

product1        :: [Int] -> Int
product1    []  = 1
product1 (n:ns) = n * product ns  

length1        :: [a] -> Int
length1   []   = 0
length1 (n:ns) = 1 + length ns 

{-   : is element concatenation not list concatenation 
reverse1        :: [a] -> [a]
reverse1     [] = []
reverse1 (n:ns) = ((reverse1 ns) : n : [])
 -}

reverse1        :: [a] -> [a]
reverse1     [] = []
reverse1 (n:ns) = (reverse1 ns) ++ [n]

zip1 ::  [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (m:ms) (n:ns) = [(m,n)] ++ (zip1 ms ns) 

listand        :: [Bool] -> Bool
listand    []  = True
listand (n:ns) = n && (listand ns)

concat2     :: [[a]] -> [a]
conact2    []  = []
concat2 (l:ls) = l ++ (concat2 ls) 

replicate1     :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n a = a : (replicate (n-1) a)

bangbang  :: [a] -> Int -> a
bangbang (n:ns) 0 = n
bangbang (n:ns) m = bangbang ns (m-1) 

elem1          :: Eq a => [a] -> a -> Bool
elem1 [] m     = False 
elem1 (n:ns) m = if m == n then True else elem1 ns m 

{- setEqual :: [Integer] -> [Integer] -> Bool -}
{- setEqual xs ys = and[x == y | x <- xs, y <- ys] -}

map'  f xs = [f x | x <- xs]

map'' f []     = []
map'' f (x:xs) = (f x) : (map'' f xs) 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x| x <- xs, f x] 

{-  filter''          :: (a -> Bool) -> [a] -> [a]
filter'' f []     = []
filter'' f (x:xs) = if f x then x:(filter'' xs) 
			   else filter'' xs  -} 


foldr' f v []     = v
foldr' f v (x:xs) = x `f` (foldr' f v xs)

{- compose       :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = \x -> f (g x) -}

odd' = not . even

type Pos = (Int,Int)

origin :: Pos
origin = (0,0)
left :: Pos -> Pos
left (x,y) = (x-1,y) 

type Pair a = (a,a)

mult :: Pair Int -> Int
mult (x,y) = x*y
copy :: a -> Pair a
copy x = (x,x)

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes,No,Yes,Yes,Unknown]

flip' :: Answer -> Answer
flip' Yes = No
flip' No = Yes
flip' Unknown = Unknown

{-p &&& q = \x -> px && qx-}

elem'          :: (Eq a) => a -> [a] -> Bool
elem' m []     = False
elem' m (n:ns) = if m == n then True else elem' m ns

setDiff [] x = []
setDiff x [] = x
setDiff (m:ms) (n:ns) = if elem' m (n:ns) then setDiff ms (n:ns) else m:(setDiff ms (n    :ns))

setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion xs ys = (setDiff xs ys) ++ ys

compl :: Integer -> [[Integer]] -> [[Integer]]
compl y yss = [[y] ++ ys | ys <- yss]

powerSet  :: [Integer] -> [[Integer]]
powerSet [] = [[]]
powerSet (x:xs) =  (powerSet xs) ++ (compl x (powerSet xs))

