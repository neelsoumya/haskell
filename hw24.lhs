\begin{code} 
        
main :: IO()
main =  
        do
		print (makeLongInt 123 10)
		print (evaluateLongInt (10,[1,2,3]))
                print (changeRadixLongInt (10,[1,2,3]) 8)
		print (changeRadixLongInt (10,[1,2,3]) 16)
		print (addLongInts (10,[1,2,3]) (3,[1]))
		print (multLongInts (10,[1,2,3]) (3,[2]))

type Numeral = (Int,[Int])

makeLongInt n r = (r, (makeLongInt' n r))

makeLongInt' 0 r = []
makeLongInt' n r = (makeLongInt' (n `div` r) r) ++ [n `mod` r]
 
evaluateLongInt (r,l) = foldr (\ (x,y) l -> x*y + l) 0 (zip (iterate (*r) 1) (reverse l))

myAdd a b = a + b

myMult a b = a * b

myIntegerMod n r
	| n < r = n
	| n == r = 0
	| otherwise = myIntegerMod (n - r) r

myIntegerDiv n r = myIntegerDiv' n r 0

myIntegerDiv' n r c
	| n < 0 = c - 1
	| otherwise = myIntegerDiv' (n - r) r (c + 1)

withoutBuiltinMakeLongInt' 0 r = []
withoutBuiltinMakeLongInt' n r = (withoutBuiltinMakeLongInt' (myIntegerDiv n r) r) ++ [myIntegerMod n r]

withoutBuiltinMakeLongInt n r = (r, (withoutBuiltinMakeLongInt' n r))

withoutBuiltinEvaluateLongInt (r,l) = foldr (\ (x,y) l -> (myAdd (myMult x y) l) ) 0 (zip (iterate (myMult r) 1) (reverse l))

changeRadixLongInt (r1, l) r2 = makeLongInt (withoutBuiltinEvaluateLongInt (r1, l)) r2


myzip [] [] = []
myzip [] (x:xs) = (0,x):(myzip [] xs)
myzip (x:xs) [] = (x,0):(myzip xs [])
myzip (x:xs)(y:ys) = (x,y):(myzip xs ys)

addLongInts (r1,l1) (r2,l2) = if r1 >= r2 then (r1, (addLong (r1,l1) (changeRadixLongInt (r2,l2) r1))) else (r2, (addLong (r2,l2) (changeRadixLongInt (r1,l1) r2)))

addLong (r,l1) (t,l2) = reverse (addLong' r  (reverse ( zip (reverse (myzip (reverse l1) (reverse l2))) (iterate (*r) 1) )))

addLong' r [] = []
addLong' r [((x,y),w)]
		| x + y >= r = [x + y - r, 1]
		| otherwise = [x + y]
addLong' r (((x1,y1),w1):(((x2,y2),w2):zs))
		| x1 + y1 >= r = ((x1 + y1 - r):(addLong' r  (((x2 + 1,y2),w2):zs)))		
		| otherwise = ((x1 + y1):(addLong' r  (((x2,y2),w2):zs)))


mulDigit x [] r z = []
mulDigit x [(y1,c1)] r z
		| x * y1 + c1 >= r = [x * y1 + c1 - r,1]
		| otherwise = [x * y1 + c1]
mulDigit x ( (y1,c1):((y2,c2):ys) ) r z
		| x * y1 + c1 >= r  = ( (x * y1 + c1 -r):( mulDigit x ((y2,1):(ys)) r z ) ) 
		| otherwise  = ( (x * y1 + c1):( mulDigit x ((y2,0):(ys)) r z ) )

stripSecondNum [] l r z = []
stripSecondNum (x:xs) l r z = ((reverse (mulDigit x (reverse(zip l (iterate (id) 0))) r z)) ++ (replicate z 0))  : (stripSecondNum xs l r (z + 1))  

multLongInts' (r,l1) (t,l2) = foldr (addLongInts) (r,[0]) (zip (repeat r) (stripSecondNum (reverse l1) l2 r 0))

multLongInts (r,l1) (t,l2)
		| r >= t = multLongInts' (r,l1) (changeRadixLongInt (t,l2) r)
		| otherwise = multLongInts' (changeRadixLongInt (r,l1) t) (t,l2)

\end{code}
