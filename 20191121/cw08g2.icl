module cw08g2
import StdEnv


// Given a tree. Write three functions for preorder, inorder and postorder traversals.
::Tree a = Node a (Tree a) (Tree a)
|Leaf

postorder :: (Tree Int) -> [Int]
postorder Leaf = []
postorder (Node x l r) = inorder l ++ inorder r ++ [x]

inorder :: (Tree Int) -> [Int]
inorder Leaf = []
inorder (Node x l r) = inorder l ++  [x]  ++ inorder r 


preorder :: (Tree Int) -> [Int]
preorder Leaf = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r



//Start = inorder (Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf) // [1,2,3,4]
// Start = postorder (Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf) // [1,3,2,4]
//Start = preorder (Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf) // [4,2,1,3]


// Write foldr function for arrays
arrFold :: (a -> b -> b) b {a} -> b
arrFold x y z =  foldr x y [m\\ m <-: z]




//Start = arrFold (+) 0 {1,2,3,4,5} // 15
//Start = arrFold (++) [] {[1],[2],[3],[4]} // [1,2,3,4]


// Write length function for arrays
arrLength :: {a} -> Int
arrLength n = length ([m \\ m <-: n])

//Start = arrLength {1,2,3} // 3
//Start = arrLength {} // 0
Start = arrLength {[2,3,4,5]} // 1
