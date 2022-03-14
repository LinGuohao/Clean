module cw09g4
import StdEnv

/*
1. Check whether a binary search tree is a degenerate case
(
  Degenerate tree: All of its nodes (for simplicity, exclude leaves) formed a straight line
  Binary search tree: For every nodes, its left node value < its value < its right node value.
)
*/

//f1 :: (Tree a) -> Bool

// Start = f1 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) // True
// Start = f1 (Node 1 Leaf (Node 2 Leaf (Node 3 Leaf Leaf))) // True
// Start = f1 (Node 1 Leaf (Node 2 Leaf (Node 1 Leaf Leaf))) // False
// Start = f1 (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) // False

//2. Define rational class Q for rational numbers. Define instances for addition and multiplication

//Start = {nom=2, den=4} + {nom=5, den=6} // (Q 4 3)
//Start = {nom=2, den=4} * {nom=5, den=6}  // (Q 5 12)


//3. Define abstract type PriorityQueue

:: Queue a :== [a]

newQueue :: (Queue a) // Creates empty queue 
newQueue = []
isempty :: (Queue a) -> Bool // Checks if a queue is empty 
isempty a
|length a == 0 = True
= False
push :: a (Queue a) -> Queue a // add an item to the end of the queue 
push x y = y ++ [x]
pop :: (Queue a) -> ((Queue a), a) | Eq,Ord a// Remove the biggest item from the queue 
//pop x = (removeAt ((length x)-1) (sort x) , bigest x)   
pop x = (filter ((<>)(bigest x)) x,bigest x)
PQ :: (Queue Int)
PQ = [1, 2, 3, 4, 5]

bigest :: (Queue a) -> a | Ord a 
bigest x = last (sort x) 
//Start = push 6 [1,2,3] //True
//Start=removeAt 2 [1,276,4,3,78]
//Start =  push 1 PQ //[1,2,3,4,5,1]
//Start =  pop PQ //([1,2,3,4,1], 5)
Start = filter ((<>)5) [1..10]