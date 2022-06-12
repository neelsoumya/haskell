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

{--(\x -> x + x) 5--}


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


data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle n) = pi * n^2


data Maybe a = Nothing | Just a

{--div1 :: Float -> Float -> Maybe Float
div1 _ 0 = Nothing
div1 m n = Just(m `div` n)--}


data Expr = Val Int | Add Expr Expr | Mul Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add m n) = (eval m) + (eval n)
eval (Mul m n) = (eval m) * (eval n)


data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node t1 n t2) = if m == n then True 
			  else occurs m t1 || occurs m t2	


data A t = L t | B (A t) (A t)

occurs' :: Int -> A Int -> Bool
occurs' m (L n) = m ==n

