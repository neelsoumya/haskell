data T = Leaf | Node T T
	deriving Show
data P = GoLeft P | GoRight P | This
	deriving Show


tree1 :: T
tree1 = Node (Leaf) (Node Leaf Leaf)
tree2 :: T
tree2 = (Node (Node Leaf Leaf) (Node Leaf Leaf))

allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node ltree rtree) =  [This] ++ (map GoLeft (allpaths ltree)) ++ (map GoRight (allpaths rtree)) 
