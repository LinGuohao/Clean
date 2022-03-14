module cw09g1
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf 

/*
1. Check if a binary tree is an ordered and balanced tree
(balanced, the difference between the depth of left and right trees is at most 1)
(ordered, nodes on the left subtree < node < nodes on the right subtree)
*/

treetolist :: (Tree a) -> [a]
treetolist Leaf = []
treetolist (Node x r l) = treetolist r ++ [x] ++ treetolist l 

depthl :: (Tree a) -> Int
depthl Leaf = 0
depthl (Node x l f)  = 1 + (depthl l) 

depthr :: (Tree a) -> Int
depthr Leaf = 0
depthr (Node x l r) = 1 + (depthr r)

f1 :: (Tree a) -> Bool |Ord , Eq a 
f1 x 
| ((abs((depthr x) - (depthl x))) < 2) && ((sort (treetolist x)) == (treetolist x)) = True
=False


//Start = f1 (Node 26 (Node 24 Leaf Leaf) (Node 28 (Node 27 Leaf Leaf) Leaf)) //True
//Start = depthr (Node 26 (Node 24 Leaf Leaf) (Node 29 (Node 27 Leaf (Node 28 Leaf Leaf)) Leaf)) //False
//Start = f1 (Node 26 (Node 24 Leaf Leaf) (Node 29 (Node 27 Leaf Leaf) (Node 28 Leaf Leaf))) //False
//Start = f1 (Node 26 (Node 31 Leaf Leaf) (Node 28 (Node 27 Leaf Leaf) Leaf)) //False

//2. Define rational class Q for rational numbers. Define instances for addition and multiplication
/*:: Q {nom :: Int , den :: Int}

instance + Q
   where + q1 q2 = {nom = q1.nom}


Start = {nom=2, den=4} + {nom=5, den=6} // (Q 4 3)
//Start = {nom=2, den=4} * {nom=5, den=6}  // (Q 5 12)
*/

//3. Define an abstract type queue

::  Queue a :==[a]

newQueue :: (Queue a) // Creates empty queue 
newQueue = []

isempty :: (Queue a) -> Bool // Checks if a queue is empty 
isempty x
|(length x == 0) = True
= False

enqueue :: a (Queue a) -> Queue a // add an item to the queue 
enqueue x y = y ++ [x]

dequeue :: (Queue a) -> Queue a |Eq a //Remove an item  from the queue 
dequeue [x:xs] = xs 

peek :: (Queue a) -> a //Gets the element at the front of the queue
peek x = (hd x)


//Start = isempty newQueue //True
//Start =  enqueue (1,2) (enqueue (6,5) (enqueue (0,9) newQueue)) //[(0,9),(6,5),(1,2)]
//Start =  peek (enqueue (1,2) (enqueue (6,5) (enqueue (0,9) newQueue))) //(0,9)
Start =  dequeue (enqueue (6,5) (enqueue (0,9) newQueue)) //[(6,5)]