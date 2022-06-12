\begin{code}

main :: IO()
main = 
        do 
		print (foldTree (*) (id) 1 tree1)
		print (foldTree (+) (id) 0 tree1)

data Tree a = Node a [Tree a]
		deriving Show

tree1 :: Tree Int
tree1 = Node 5 [ Node 1 [], Node 2 [ Node 3 [], Node 4 [Node 6 []] ] ]


foldTree1 fNode fLeaf value ([Node x []]) = fLeaf x
foldTree1 fNode fLeaf value ([]) = value
foldTree1 fNode fLeaf value ((Node x (y:ys)):ls) = fNode (fNode x (fNode (foldTree1 fNode fLeaf value [y]) (foldTree1 fNode fLeaf value ys) ))  (foldTree1 fNode fLeaf value ls)
foldTree fNode fLeaf value (Node x ys)  = foldTree1 fNode fLeaf value [Node x ys]

mapTree f = foldTree ( \[(Node x (y:ys))] ls -> ((Node (f x) (y:ys) ):ls) ) ( \([Node x []]) -> ([Node (f x) []]) ) 0




\end{code}
