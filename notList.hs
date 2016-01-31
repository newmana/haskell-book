data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right 
  | b > a = Node left a (insert' b right)
    
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = buildTree f (f x)
  where
    buildTree _ Nothing = Leaf
    buildTree f (Just (a1, b, a2)) = Node (unfold f a1) b (unfold f a2) 

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold (doStuff' x) 0

unfold' :: (Ord b) => (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold' f x = buildTree' f (f x)
  where
    buildTree' _ Nothing = Leaf 
    buildTree' f (Just(a1, b, a2)) = insert' b $ Node (unfold' f a1) b (unfold' f a2) 
    
treeBuild' :: Integer -> BinaryTree Integer
treeBuild' x = unfold' (doStuff' x) 0

doStuff :: (Num a, Ord a) => a -> Maybe (a, a, a)
doStuff x = if x > 0 then Just (x-1, x-1, x-1) else Nothing

doStuff' :: (Num a, Ord a) => a -> a -> Maybe (a, a, a)
doStuff' max x = if x < max then Just (x+1, x, x+1) else Nothing

answer2 = Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
answer3 = Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))