data BinaryTree a = 
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf 
insert' b (Node left a right) 
  | b == a = Node left a right 
  | b < a = Node (insert' b left) a right 
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)
preorder Leaf = [] 

inorder :: BinaryTree a -> [a]
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)
inorder Leaf = [] 

postorder :: Ord a => BinaryTree a -> [a]
postorder (Node left a right) = (inorder left) ++ (inorder right) ++ [a] 

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f acc left) (foldTree f acc right)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree (\v lt yt -> Node lt (f v) yt) Leaf bt

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
