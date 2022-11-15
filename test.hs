module Test where
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = putStrLn "test.hs"

bmiCounter::(RealFloat a)=> a->a-> String
bmiCounter weight height
 | weight / height^2 <=18.5 = "underweight"
 | weight / height^2 <=25.0 = "normal"
 | weight / height^2 <=30 = "overweight"
 | otherwise = "you are a whale"


workingDay::(Integral a)=> a-> String
workingDay 1 = "Monday"
workingDay 2 = "Tuesday"
workingDay 3 = "Wednesday"
workingDay 4 = "Thursday"
workingDay 5 = "Friday"
workingDay x = "Holiday"

{-
符号=>，其左边的东西叫类型约束（Type constraints ），
一个类型声明可以看做两段，=>右边的部分是类型，左边的部分约束了类型变量必须属于的类型类
-}
{-
typeclass:
   Eq是可判断相等性的类型类，提供== /=函数，除函数以外所有类型都实现了这个类型类
   Ord是可比较大小的类型类，提供< > <= >=之类用于比较大小的函数
   compare函数用于两个同类Ord的比较，类型是Ord a => a -> a -> Ordering，结果是以下三个值之一：LT EQ GT，并具有大小关系LT < EQ < GT
   Show是成员可用字符串表示的类型类。常用函数是show，将类型转换为[Char]/String
   Read是Show相反的类型类，read将一个字符串转换为Read的实例类型
   Enum的类型类的实例都是可枚举类型，属于Enum类型类的类型可以用于Range中。每个值都有后继（successer）和前置（predecesor），可分别通过suc和pred得到。包含类型有：() Bool Char Ordering Int Integer Float Double。
   Bounded类型类都有一个上限和下限。minBound maxBound的返回类型是Bounded a => a，无参数，得到一个Bounded类型的下限和上限
   Num表示数字。包括所有实数和整数
   Integral是表示整数的类型类，包含Int Integer
   Floating表浮点数，包含Float Double
   romIntegral函数处理数字时很有用，类型是(Integral a, Num b) => a -> b从整数提取出一个更通用的Num。比如当length [1, 2] * 5的*类型是Int -> Int没有问题，但length [1, 2] * 5.0则会类型不匹配
-}

{-
  data 类型名 = 值构造器
  deriving (Eq,Show)把实例变成可以相等用字符串表示的实例
  在类型后加上类型参数可以实现泛型的功能，比如Map k a，键和值的类型是类型的一部分。是对应于C++模板、java泛型之类的语法。比如Maybe：
  for example:
     data Maybe a = Nothing | Just a
     有了类型参数a后，Maybe就不再是类型，Maybe a整体才是一个类型，Maybe则称为类型构造器：传入类型参数就可以得到类
-}
data Shape = Circle Float


surface::Shape->Float
surface (Circle r) = pi*r^2

-- add assignment
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
   

isLeafRB :: RBT a -> Bool
isLeafRB LeafRB = True
isLeafRB _ = False

{-
  Maybe a整体上是一个数据类型
  :i Maybe
    type Maybe :: * -> *
    data Maybe a = Nothing | Just a
-}
-- Minimum and maximum of red-black tree   
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


-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isGreaterOrEq :: Ord a=> a-> RBT a-> Bool
isGreaterOrEq  a (NodeRB _ _ b _) = a >=b
isGreaterOrEq  a LeafRB = True

isLessOrEq :: Ord a => a -> RBT a -> Bool
isLessOrEq a (NodeRB _ _ b _) = a<=b
isLessOrEq a LeafRB = True


isBST :: Ord a => RBT a -> Bool
isBST leafRB = False
isBST (NodeRB _ leftNode key rightNode) 
    | key `isGreaterOrEq` leftNode && key `isLessOrEq` rightNode = isBST leftNode && isBST rightNode
    | otherwise = False
    

--type LeafToBlack a = (RBT a,String)
--type AllLeaf a = [LeafToBlack a]

addOne :: Integral a=>a->a
addOne a = a + 1

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

{-
detectLeaf :: (Ord a, Integral bc)=> (RBT a,bc,AllLeaf a)-> AllLeaf a
detectLeaf (NodeRB Black leftNode key rightNode, blackCount,alf)
  | leftNode == LeafRB && rightNode ==LeafRB = (NodeRB Black leftNode key rightNode, blackCount):alf
  | leftNode /= LeafRB = detectLeaf (leftNode,blackCount,alf)
  | rightNode /=LeafRB = detectLeaf (rightNode,blackCount,alf)
detectLeaf (NodeRB Red leftNode key rightNode, blackCount,alf)
  | leftNode == LeafRB && rightNode ==LeafRB = (NodeRB Black leftNode key rightNode,blackCount):alf
  | leftNode /= LeafRB = detectLeaf (leftNode,blackCount,alf)
  | rightNode /=LeafRB = detectLeaf (rightNode,blackCount,alf)
-}






-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

blackBalanced :: RBT a -> Bool
blackBalanced LeafRB = False
blackBalanced (NodeRB Black leftNode key rightNode) =
    isEqualList (detectLeaf(NodeRB Black leftNode key rightNode,0,[]))

-- Black height of a black-balanced tree, -1 if not black-balanced

blackHeight :: RBT a -> Int
blackHeight LeafRB = 0
blackHeight (NodeRB Black leftNode key rightNode) =
    maximum (detectLeaf(NodeRB Black leftNode key rightNode,0,[]))

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

-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool
isRBT LeafRB = False
isRBT (NodeRB Red _ _ _) = False
isRBT (NodeRB Black leftNode key rightNode)
 | blackBalanced (NodeRB Black leftNode key rightNode) && isAllChildrenBlack (NodeRB Black leftNode key rightNode) = True
 | otherwise = False


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
-- case father node is grandfather node's left child
-- 
--balanceRBT father @(NodeRB color_father (NodeRB Red LeafRB _ LeafRB) _ _) grandfather @(NodeRB color_grandfather father _ uncle) uncle @(NodeRB color_uncle _ _ _)
--  case 1: both father and uncle node are red
 -- | color_father == color_uncle == Red = 
-- Insert a new element in a RBT, preserving the RBT properties


insertRB' :: Ord a => a -> RBT a -> RBT a
insertRB' a LeafRB = NodeRB Red LeafRB a LeafRB
insertRB' a t@(NodeRB color leftNode key rightNode)
 | a< key = balance (NodeRB color (insertRB' a leftNode) key  rightNode)
 | a> key = balance (NodeRB color leftNode key (insertRB' a rightNode))
 | otherwise = t

insertRB :: Ord a => a -> RBT a -> RBT a
insertRB a t@(NodeRB color leftNode key rightNode) = blackRoot(insertRB' a t)


-- Delete an element from a RBT, preserving the RBT properties

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


{-
fuse :: Ord a=>RBT a -> RBT a -> RBT a
fuse t1@(NodeRB color_t1 t3 _ t4) t2@(NodeRB color_t2 t5 _ t6)
  | color_t1/=color_t2 = 
    if color_t1 == Red
      then t1 @(NodeRB color_t1 t3 _ (fuse t4 t2))
    else t2 @(NodeRB color_t2 (fuse t1 t5) _ t6)
  | color_t1 == color_t2 == Red
    if getColor s == Black
      then t1 @(NodeRB color_t1 t3 _ (NodeRB color_t2 s _ t6))
    else t1
  | color_t1 == color_t2 == Black
    if getColor s == Red then 
      t1 @(NodeRB color_t1 t3 _ (NodeRB color_t2 s _ t6))
    else t1
  where s = fuse t4 t5
-}
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



del :: Ord a=> a -> RBT a  -> RBT a
del x LeafRB = LeafRB
del x (NodeRB _ t1 y t2)
  | x<y = deleteLeft x t1 y t2
  | x>y = deleteRight x t1 y t2
  | otherwise = fuse t1 t2

deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB x t = blackRoot (del x t)