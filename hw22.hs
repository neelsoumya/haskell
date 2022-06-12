data Tree a = Node a [Tree a]
		deriving Show

tree1 :: Tree Int
tree1 = Node 5 [ Node 1 [], Node 2 [ Node 3 [], Node 4 [Node 6 []] ] ]


{-foldTree fNode fLeaf value [] = value
-}
foldTree1 fNode fLeaf value ([Node x []]) = fLeaf x
{-
foldTree fNode fLeaf value ((Node x (y:ys)):ls) = fNode x (fNode (foldTree fNode fLeaf value [y]) (foldTree fNode fLeaf value ys) )
-}
foldTree1 fNode fLeaf value ([]) = value
foldTree1 fNode fLeaf value ((Node x (y:ys)):ls) = fNode (fNode x (fNode (foldTree1 fNode fLeaf value [y]) (foldTree1 fNode fLeaf value ys) ))  (foldTree1 fNode fLeaf value ls)
foldTree fNode fLeaf value (Node x ys)  = foldTree1 fNode fLeaf value [Node x ys]

{-foldTree (+) (id) 0 tree1-}

{-
tree2 :: Tree Int
tree2 = Node 4 []
foldTree fNode value (Node x []) = x
foldTree fNode value (Node x (y:ys)) = fNode x (foldTree1 fNode value (y:ys))

foldTree1 fNode value ((Node x l1):l2) = fNode  (fNode x (foldr fNode value (map (foldTree fNode value) l1) ))    (foldr fNode value (map (foldTree fNode value) l2)) 
-}
mapTree f = foldTree ( \[(Node x (y:ys))] ls -> ((Node (f x) (y:ys) ):ls) ) ( \([Node x []]) -> ([Node (f x) []]) ) 0
{-
mapTree f = foldTree ( \x y -> ((Node (f x)):y) ) ( \x -> (Node (f x) []) ) 0
-}
