module C3JI5D_Homework10
import StdEnv

::Tree a = Node a (Tree a) (Tree a)|Leaf

testTree = Node 4 (Node 2 (Node 6 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 (Node 7 (Node 3 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 2 (Node 3 Leaf Leaf) (Node 1 (Node 9 (Node 10 (Node 11 (Node 5 Leaf Leaf) (Node 8 Leaf Leaf)) Leaf) Leaf) (Node 5 Leaf Leaf))))
//Start = testTree

// For ex 1,2,3 use the Tree Int type defined in the class.

// 1. Check if a number is a node of a tree.
f1 :: (Tree Int)  -> [Int]
f1 Leaf = []
f1 (Node x l r) = [x] ++ f1 l ++ f1 r

f1helper :: [Int] Int -> Bool
f1helper  x y 
|isMember y x == True = True
= False

isNode :: (Tree Int) Int -> Bool
isNode x y = f1helper (f1 x) y


//Start = isNode testTree 1337



// 2. Count how many times is a node in a tree.
f2 :: (Tree Int ) Int  -> Int
f2 x y =  length[z\\z <- (f1 x) | y == z]



//Start = f2 testTree 2

// 3. If a number is an a tree, then give the list of its children in the form of (left child, right child).

// if the children are leaves then (0,0) are its children.

nodeReturn :: (Tree Int) -> (Int,Int)
nodeReturn (Node x Leaf Leaf) = (0,0)
nodeReturn (Node x (Node y _ _)(Node m _ _)) = (y,m)
nodeReturn (Node x Leaf (Node m _ _)) = (0,m)
nodeReturn (Node x (Node y _ _) Leaf) = (y,0)




//Start = nodeReturn  testTree
 
child :: (Tree Int) Int -> [(Int, Int)]
child Leaf _ = []
child (Node x l r) y
|x == y = [ nodeReturn (Node x l r) ] ++ child l y ++ child r y
= child l y ++ child r y




Start = child testTree 3

// eg. for aTree = Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 2 Leaf Leaf)

// has 2 as node two times, and the result for Start = child aTree 2 is [(1,3),(0,0)]