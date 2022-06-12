\begin{code}

main :: IO()
main = 
	do
		print (isTree graph1)
		print (isTree graph2)
		print (isDAG graph1)
		print (isDAG graph2)
		print (isDAG graph3)
		print (isRootedDAG graph1)
		print (isRootedDAG graph2)
		print (isRootedDAG graph3)
		print (calcDepth graph1)

type Graph = [(Int,Int)]
	

graph1 :: Graph
graph1 = [(1,2),(2,3),(1,4)]

graph2 :: Graph
graph2 = [(1,2),(2,3),(1,4),(4,2)]

graph3 :: Graph
graph3 = [(1,2),(2,3),(1,4),(4,2),(2,1)]

findVertices v zs = [y | (x,y) <- zs, x == v]

dfs :: [Int] -> Graph -> [Int]
dfs [] l = []
dfs (y:ys) l = [y] ++ (dfs (findVertices y l) l) ++ (dfs ys l)


removeDuplicates [] = []
removeDuplicates (x:xs) = x : (removeDuplicates (filter (\y -> not(x == y)) xs))


makeVertexList l = removeDuplicates ((fst (unzip l)) ++ (snd(unzip l)))

{- takes the vertex list and the original graph and returns the list of list of vertices reached by dfs from each vertex -}
iterateDfs xs l = [[x] ++ (removeDuplicates(dfs (findVertices x l) l)) | x <- xs]


mysort [] = []
mysort (x:xs) = mysort y1 ++ [x] ++ mysort y2
		where 
			y1 = [p | p <- xs, p <= x]
			y2 = [q | q <- xs, q > x]

{- will take the list of list of vertices reached by dfs from each vertex and the list of vertices. If any of vertices has reached all vertices, the return true -}
connectedTest ls l2 = or[True  | l1 <- ls, (mysort l1) == (mysort l2)]

{- the graph is a tree if the graph is connected and the number of vertices is one less than the number of edges -}
isTree :: Graph -> Bool
isTree g = (length (vl)) == (1 + length (g)) &&  (connectedTest (iterateDfs vl g) vl)
		where
			vl = makeVertexList g




isDAG :: Graph -> Bool
isDAG g = and[ not(v `elem` (dfs (findVertices v g)  g)) | v <- ( removeDuplicates ((fst (unzip g)) ++ (snd(unzip g))) )] 

{- find a list of vertices in the graph with indegree 0 -}
findIndegree0 zs = removeDuplicates ([ x | (x,y) <- zs, not (x `elem` (map snd zs)) ])

{- if there is at least one vertex with outdegree 0 and there is one vertex from which all other vertices are reachable, then the graph is a rooted DAG-}
isRootedDAG' g vs l = ((length vs) > 0) && (or [ (mysort ( [v] ++ (removeDuplicates(dfs (findVertices v g) g)) )) == (mysort l) | v <- vs ])


isRootedDAG :: Graph -> Bool
isRootedDAG [] = True
isRootedDAG g = isRootedDAG' g (findIndegree0 g) (makeVertexList g)


dfsDepth [] v g dist = []
dfsDepth (u:us) v g dist 
		| not (u == v) =  (dfsDepth (findVertices u g) v g (dist + 1)) ++ (dfsDepth us v g (dist + 1))
		| otherwise = [dist]

formTuple r g = [(v,head(mysort (dfsDepth r v g 0))) | v <- (makeVertexList g)]

findRoot' g vs l = [v | v <- vs, (mysort ( [v] ++ (removeDuplicates(dfs (findVertices v g) g)) )) == (mysort l)] 

findRoot g = findRoot' g (findIndegree0 g) (makeVertexList g)

calcDepth g = formTuple (findRoot g) g

\end{code}
