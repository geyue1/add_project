{-
  G54PAD Project in Advanced Algorithms and Data Structures
    Autumn 2022

  Assignment 2 
    Graph Algorithms

  Student Name: Yue Ge
  Student ID: 20470967

  Complete this Haskell file by providing definitions
  of the following functions:

  adjList
  adjMatrix

  adjListW
  adjMatrixW

  dijkstra
  floydWarshall

  You are allowed to define any other auxiliary function you need.

-}


module Graphs where

import Numeric
import Data.List (sort,delete,elemIndex)
import Data.Maybe ( isNothing, isJust )

-- We assume that the vertices are numbered 0 ... n-1

{- In and adjacency list al, the i-th element al!!i
   is a list of all the vertices j such that
   there is an edge from i to j
-}

type AdjList = [[Int]]

{- In and adjacencly matrix am, the element with
   coordinates i,j, that is am!!i!!j
   is True if there is an edge from i to j
      False if there is no edge from i to j
-}

type AdjMatrix = [[Bool]]

-- Suppose we're given a graph as a list of edges (i,j)
-- Generate the Adjacency List and Adjacencty Matrix representations

-- GENERATION OF ADJACENCY LIST

adjList :: [(Int,Int)] -> AdjList
adjList [] = []
adjList ll =
    let (x:y:ys) = sort ll
    in mergeAdjList (fst x) [[snd x]] (y:ys)

mergeAdjList :: Int -> AdjList ->[(Int,Int)]-> AdjList
mergeAdjList k a [] = a
mergeAdjList k a [(i,j)]
    | k== i = init a ++ [last a++[j]]
    | otherwise = a++[[j]]
mergeAdjList k a ll = 
    let (x:y:ys) = sort ll in
    mergeAdjList (fst x) (mergeAdjList k a [x]) (y:ys)

-- GENERATION OF ADJACENCY MATRIX

adjMatrix :: [(Int,Int)] -> AdjMatrix
adjMatrix [] = []
adjMatrix ll =
   let (x:xs) = sort ll
   in setMatrixValue (buildMatrix ll) ll

buildMatrix :: [(Int,Int)] -> AdjMatrix
buildMatrix ll 
  | n>0 = replicate (n+1) (replicate (n+1) False)
  | otherwise = [[False]]
  where n = max (maximum (map fst ll)) (maximum (map snd ll))

setMatrixValue :: AdjMatrix -> [(Int,Int)] -> AdjMatrix
setMatrixValue m [] = m
setMatrixValue m [(i,j)] =
    let list = m!!i
    in take i m ++ [take j list ++ [True] ++ drop (j+1) list] ++ drop (i+1) m
setMatrixValue m ll = 
    let (x:y:ys) = sort ll
    in setMatrixValue (setMatrixValue m [x]) (y:ys)


--------------------------------------------------------

-- WEIGHTED GRAPHS: every edge has a "weight"

{- In an adjacency list al, the i-th element al!!i
   contains all the pairs (j,w) such that
   there is an edge from i to j with weight w
-}

type WAdjList = [[(Int,Float)]]

{- In an adjacency matrix am, the element with
   coordinates i,j is
     Nothing if there is no edge from i to j
     (Just w) if there is an edge from i to j with weight w
-}

type WAdjMatrix = [[Maybe Float]]

{- We can also represent a weighted graphs by a list of edges
   (i,j,w) denotes an edge from i to j with weight w
-}

type Edges = [(Int,Int,Float)]

-- GENERATION OF ADJACENCY LIST
--   from a list of edges

adjListW :: Edges -> WAdjList
adjListW [] = []
adjListW ll =
    let (x:y:ys) = sort ll
    in mergeAdjListW (fstInTriple x) [[sndInTriple x]] (y:ys)

mergeAdjListW :: Int -> WAdjList -> [(Int,Int,Float)] -> WAdjList
mergeAdjListW k wa [] = wa
mergeAdjListW k wa [(i,j,f)]
  | k==i = init wa ++ [last wa ++[(j,f)]]
  | otherwise = wa ++ [[(j,f)]]
mergeAdjListW k wa ll = 
    let (x:y:ys) = sort ll
    in mergeAdjListW (fstInTriple x) (mergeAdjListW k wa [x]) (y:ys)

fstInTriple :: (a,b,c) -> a
fstInTriple (x,_,_) = x

sndInTriple :: (a,b,c)->(b,c)
sndInTriple (_,x,y) = (x,y)


-- GENERATION OF ADJACENCY MATRIX
--   from a list of edges

adjMatrixW :: Edges -> WAdjMatrix
adjMatrixW [] = []
adjMatrixW ll =
   let (x:xs) = sort ll
   in setMatrixWValue (buildMatrixW ll) ll


buildMatrixW :: Edges -> WAdjMatrix
buildMatrixW ll 
  | n>0 = replicate (n+1) (replicate (n+1) Nothing)
  | otherwise = [[Nothing]]
  where n = max (maximum (map fstInTriple ll)) (maximum (map fst (map sndInTriple ll)))

setMatrixWValue :: WAdjMatrix -> Edges -> WAdjMatrix
setMatrixWValue wam [] = wam
setMatrixWValue wam [(i,j,f)] =
    let list = wam!!i
    in take i wam ++ [take j list ++ [Just f] ++ drop (j+1) list] ++ drop (i+1) wam
setMatrixWValue wam ll = 
    let (x:y:ys) = sort ll
    in setMatrixWValue (setMatrixWValue wam [x]) (y:ys)

-- DIJKSTRA'S ALGORITHM

{- 
   Given an adjacencly list al and a source vertex s
   return the list of minimum distances of vertices from s:
   (dijkstra al s)!!j is the minimum distance from s to j
-}

type DijkstraList = [(Int,Maybe Float)]
dijkstra :: WAdjList -> Int -> [Maybe Float]
dijkstra [] _ = []
dijkstra wa i
  | length wa -1 < i || i<0 = []
  | otherwise = map snd (sort (dijkstra' wa  djkList))
  where djkList = initDijkstraList wa i

dijkstra' :: WAdjList -> DijkstraList -> [(Int,Maybe Float)]
dijkstra' wa  [] = []
dijkstra' wa  djkList = 
    let [(a,b)] = minimumDijkstraList djkList
        afterRelaxDjkList = relaxDijkstraList wa (delete (a,b) djkList) (a,b)
    in (a,b) : dijkstra' wa afterRelaxDjkList

initDijkstraList :: WAdjList -> Int -> DijkstraList
initDijkstraList [] i = []
initDijkstraList wa i = 
   let x = wa!!i 
       k = length wa-1
   in [(m,Nothing) | m<-[0..k],m `notElem` map fst x] ++ zip (map fst x) (map (Just . snd)x)
  
minimumDijkstraList :: DijkstraList -> [(Int,Maybe Float)]
minimumDijkstraList [] = []
minimumDijkstraList [(a,Nothing)] = []
minimumDijkstraList [(a,Just b)] = [(a,Just b)]
minimumDijkstraList ((a,b):xs)
  | isNothing b= minValue
  | not (null minValue) && (b > snd (head minValue)) = minValue
  | otherwise = [(a,b)]
  where minValue = minimumDijkstraList (xs)

relaxDijkstraList :: WAdjList -> DijkstraList -> (Int,Maybe Float) -> DijkstraList
relaxDijkstraList wa djkList (a,Nothing) = djkList
relaxDijkstraList wa djkList (a,Just f) = updateWeight f (wa!!a) djkList


{-
elemIndex :: Eq a => a -> [a] -> Maybe Int
-}
updateWeight :: Float -> [(Int,Float)] -> DijkstraList -> DijkstraList
updateWeight a [] djkList = djkList
updateWeight a [(x_1,x_2)] djkList
  | isNothing (elemIndex x_1 fstDjk) = djkList
  | isNothing (sndDjk!!index)|| (sndDjk!!index > Just (x_2+a)) = take index djkList ++ [(x_1,Just (x_2+a))] ++ drop (index+1) djkList
  | otherwise = djkList
  where fstDjk = map fst djkList
        sndDjk = map snd djkList
        Just index = elemIndex x_1 fstDjk
updateWeight a (x:xs) djkList = updateWeight a (xs) (updateWeight a [x] djkList)

-- FLOYD-WARSHALL ALGORITHM

{-
   Given an adjacency matrix am, return the matrix of minimum distances:
   (floydWarshall am)!!i!!j is
     Nothing if there is no path from i to j
     (Just x) if the shortest path from i to j has length x
-}

floydWarshall :: WAdjMatrix -> WAdjMatrix
floydWarshall [] = []
floydWarshall wam = 
   let vertices = [0 .. length wam -1]
   in floydWarshall' wam  vertices


floydWarshall' :: WAdjMatrix -> [Int] -> WAdjMatrix
floydWarshall' wam [] = wam
floydWarshall' wam (k:ks) = floydWarshall' (floydWarshall'' wam k [0..length wam -1]) (ks)

floydWarshall'' :: WAdjMatrix -> Int -> [Int] -> WAdjMatrix
floydWarshall'' wam _ [] = wam
floydWarshall'' wam k (i:is) = floydWarshall'' (floydWarshall''' wam k i [0..length wam -1]) k (is)

--type WAdjMatrix = [[Maybe Float]]
floydWarshall''' :: WAdjMatrix -> Int -> Int -> [Int] -> WAdjMatrix
floydWarshall''' wam _ _ [] = wam
floydWarshall''' wam k i [j]
  | isNothing (wam!!i!!k)  = wam
  | isNothing (wam!!k!!j)  = wam
  | isNothing distance_i_j || Just dist < distance_i_j = take i wam ++ [take j list ++ [Just dist] ++ drop (j+1) list] ++ drop (i+1) wam
  | otherwise = wam
  where Just distance_i_k = wam!!i!!k
        Just distance_k_j = wam!!k!!j
        dist = distance_i_k+distance_k_j
        distance_i_j = wam!!i!!j
        list = wam!!i
floydWarshall''' wam k i (j:js) = floydWarshall''' (floydWarshall''' wam k i [j]) k i (js)

