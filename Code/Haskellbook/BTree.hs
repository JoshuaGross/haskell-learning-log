module BTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree a b = foldr a b . inorder

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTree' :: BinaryTree Int
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Int
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if fmap (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

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

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- generate an infinite tree:
-- inorder  $ unfold (\x -> Just (x + 1, x, x + 1)) 0
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x =
  case f x of Nothing      -> Leaf
              Just (a,b,c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0
