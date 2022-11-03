{-
  COMP4040 Project in Advanced Algorithms and Data Structures
    Autumn 2022

  Assignment 1 
     Red-Balck Trees

  Student Name: Yue Ge
  Student ID: 20470967

  Complete this Haskell file by providing definitions
  of the following functions (do not change their types):

    searchRB

    minRB
    maxRB

    isBST

    blackBalanced
    blackHeight

    insertRB
    deleteRB

  You are allowed to define any other auxiliary function you need.

-}

module RedBlack where

-- Definition of the Red-Black Tree data structure

data Color = Red | Black
  deriving (Eq,Show)

data RBT a = LeafRB | NodeRB Color (RBT a) a (RBT a)
  deriving (Eq,Show)


-- Serching a key inside a red-black tree
--   return True if the key is found, False otherwise

searchRB :: Ord a => a -> RBT a -> Bool
searchRB a LeafRB = False
searchRB a (NodeRB _ leftNode key rightNode)
  | a < key = searchRB a leftNode
  | a > key = searchRB a rightNode
  | a == key = True
  | otherwise = False

-- Minimum and maximum of red-black tree
--   return Nothing if the tree is empty
minRB :: RBT a -> Maybe a
minRB LeafRB = Nothing
minRB (NodeRB _ leftNode key _)
   | isLeafRB leftNode = Just key
   | otherwise = minRB leftNode

maxRB :: RBT a -> Maybe a
maxRB leafRB = Nothing
maxRB (NodeRB _ _ key rightNode) 
   | isLeafRB rightNode = Just key
   | otherwise = maxRB rightNode

isLeafRB :: RBT a -> Bool
isLeafRB LeafRB = True
isLeafRB _ = False
  

-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isBST :: Ord a => RBT a -> Bool
isBST leafRB = False
isBST (NodeRB _ leftNode key rightNode) 
    | key `isGreaterOrEq` leftNode && key `isLessOrEq` rightNode = isBST leftNode && isBST rightNode
    | otherwise = False

isGreaterOrEq :: Ord a=> a-> RBT a-> Bool
isGreaterOrEq  a (NodeRB _ _ b _) = a >=b
isGreaterOrEq  a LeafRB = True

isLessOrEq :: Ord a => a -> RBT a -> Bool
isLessOrEq a (NodeRB _ _ b _) = a<=b
isLessOrEq a LeafRB = True

-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

blackBalanced :: RBT a -> Bool
blackBalanced LeafRB = False
blackBalanced (NodeRB Black leftNode key rightNode) =
    isEqualList (detectLeaf(NodeRB Black leftNode key rightNode,0,[]))

detectLeaf :: (Integral b,Ord b)=> (RBT a,b,[b])->[b]
detectLeaf (NodeRB Black leftNode key rightNode, blackCount,alf)
  | not (isLeafRB leftNode) = detectLeaf (leftNode,blackCount,alf)
  | not (isLeafRB rightNode) = detectLeaf (rightNode,blackCount,alf)
  | isLeafRB leftNode && isLeafRB rightNode  = blackCount+1:alf
detectLeaf (NodeRB Red leftNode key rightNode, blackCount,alf)
  | isLeafRB leftNode && isLeafRB rightNode = blackCount:alf
  | not (isLeafRB leftNode) = detectLeaf (leftNode,blackCount,alf)
  | not (isLeafRB rightNode) = detectLeaf (rightNode,blackCount,alf) 

isEqualList :: Integral a=> [a]-> Bool
isEqualList a = minimum a == maximum a

-- Black height of a black-balanced tree, -1 if not black-balanced

blackHeight :: RBT a -> Int
blackHeight LeafRB = 0
blackHeight (NodeRB Black leftNode key rightNode) =
    maximum (detectLeaf(NodeRB Black leftNode key rightNode,0,[]))

-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool
isRBT LeafRB = False
isRBT (NodeRB Red _ _ _) = False
isRBT (NodeRB Black leftNode key rightNode)
 | blackBalanced (NodeRB Black leftNode key rightNode) && isAllChildrenBlack (NodeRB Black leftNode key rightNode) = True
 | otherwise = False

getColor :: RBT a -> Color
getColor LeafRB = Black
getColor (NodeRB color _ _ _)
  | color == Red = Red
  | color == Black = Black

isAllChildrenBlack :: RBT a -> Bool
isAllChildrenBlack LeafRB = True
isAllChildrenBlack (NodeRB Red leftNode _ rightNode)
 | getColor leftNode == Black && getColor rightNode == Black = 
     isAllChildrenBlack leftNode && isAllChildrenBlack rightNode
 | otherwise = False
isAllChildrenBlack (NodeRB Black leftNode _ rightNode) = 
    isAllChildrenBlack leftNode && isAllChildrenBlack rightNode

-- Insert a new element in a RBT, preserving the RBT properties

insertRB :: Ord a => a -> RBT a -> RBT a
insertRB a t@(NodeRB color leftNode key rightNode) = blackRoot(insertRB' a t)

insertRB' :: Ord a => a -> RBT a -> RBT a
insertRB' a LeafRB = NodeRB Red LeafRB a LeafRB
insertRB' a t@(NodeRB color leftNode key rightNode)
 | a< key = balance (NodeRB color (insertRB' a leftNode) key  rightNode)
 | a> key = balance (NodeRB color leftNode key (insertRB' a rightNode))
 | otherwise = t

blackRoot :: Ord a => RBT a -> RBT a
blackRoot LeafRB = LeafRB
blackRoot a@(NodeRB color l key r )
  | color == Red = NodeRB Black l key r
  | otherwise = a


balanceRBT :: Ord a => RBT a -> RBT a
balanceRBT (NodeRB Black (NodeRB Red (NodeRB Red a x b) y c) z d) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d) 
balanceRBT (NodeRB Black (NodeRB Red a x (NodeRB Red b y c)) z d) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balanceRBT (NodeRB Black a x (NodeRB Red (NodeRB Red b y c) z d)) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balanceRBT (NodeRB Black a x (NodeRB Red b y (NodeRB Red c z d))) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balance a = a

-- Delete an element from a RBT, preserving the RBT properties

deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB x t = blackRoot (del x t)

del :: Ord a=> a -> RBT a  -> RBT a
del x LeafRB = LeafRB
del x (NodeRB _ t1 y t2)
  | x<y = deleteLeft x t1 y t2
  | x>y = deleteRight x t1 y t2
  | otherwise = fuse t1 t2

fuse :: Ord a=>RBT a -> RBT a -> RBT a
fuse t1@(NodeRB color_t1 t3 key_t1 t4) t2@(NodeRB color_t2 t5 key_t2 t6)
  | color_t1/=color_t2 = 
    if color_t1 == Red
      then NodeRB color_t1 t3 key_t1 (fuse t4 t2)
    else NodeRB color_t2 (fuse t1 t5) key_t2 t6
  | color_t1 == Red && color_t2 == Red =
    if color_s == Black
      then NodeRB color_t1 t3 key_t1 (NodeRB color_t2 s key_t2 t6)
    else NodeRB color_s (NodeRB color_t1 t3 key_t1 s1) key_s (NodeRB color_t2 s2 key_t2 t6)
  | color_t1 ==Black && color_t2 == Black =
    if color_s == Black then 
      NodeRB color_t1 t3 key_t1 (NodeRB color_t2 s key_t2 t6)
    else NodeRB color_s (NodeRB color_t1 t3 key_t1 s1) key_s (NodeRB color_t2 s2 key_t2 t6)
  where s@(NodeRB color_s s1 key_s s2 )= fuse t4 t5

balanceLeft :: Ord a=>RBT a -> a -> RBT a -> RBT a
balanceLeft (NodeRB Red t1 x t2) y t3 = NodeRB Red (NodeRB Black t1 x t2) y t3
balanceLeft t1 y (NodeRB Black t2 z t3) = balanceRBT (NodeRB Black t1 y (NodeRB Red t2 z t3))
balanceLeft t1 y (NodeRB Red (NodeRB Black t2 u t3) z t4) = NodeRB Red (NodeRB Black t1 y t2) u (balanceRBT (NodeRB Black t3 z t4))

deleteLeft :: Ord a=> a -> RBT a-> a-> RBT a -> RBT a
deleteLeft x t1 y t2 
  | getColor t1 == Black = balanceLeft (del x t1) y t2
  | otherwise = NodeRB Red (del x t1) y t2

balanceRight :: Ord a=>RBT a -> a -> RBT a -> RBT a
balanceRight t3 y (NodeRB Red t1 x t2) = NodeRB Red  t3 y (NodeRB Black t1 x t2)
balanceRight (NodeRB Black t2 z t3) y t1 = balanceRBT (NodeRB Black (NodeRB Red t2 z t3) y t1)
balanceRight (NodeRB Red (NodeRB Black t2 u t3) z t4) y t1 = NodeRB Red (balanceRBT (NodeRB Black t3 z t4)) u (NodeRB Black t1 y t2)

deleteRight :: Ord a=> a -> RBT a-> a-> RBT a -> RBT a
deleteRight x t1 y t2 
  | getColor t2 == Black = balanceRight t1 y (del x t2)
  | otherwise = NodeRB Red t1 y (del x t2)