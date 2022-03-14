module cw10g2
import StdEnv

// Define an instance of the built-in class PlusMin (class PlusMin a | + , - , zero a)
// for lists [a] such that, the addition of two lists takes place elementwise 
// (if necessary, the shortest list is extended with zeros to
// obtain two lists of equal length). So, [1,2,3] + [4,5] = [5,7,3].
instance + [Int] 
where
    
    (+) a b =  f1 a b
    
    
f1 :: [Int] [Int] -> [Int]
f1 x y 
|y == [] = x
|x == [] = y
= [(hd x) + (hd y)] ++ f1 (tl x) (tl y)        



Start = [1,2,3] + [4,5] // [5,7,3]
//Start = [1,2,3] + [1,2,3] // [2,4,6]
//Start = [1,2,3] + [] // [1,2,3]


// For a given n generate an array like
// for n=4  the result is {1,1,2,1,2,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1}
helper:: Int -> [Int]
helper 1 = [1]
helper n = (repeatn n n) ++ helper (n - 1)

f :: Int -> {Int}
f n =   {z\\z <-(flatten [([x]++[1]) \\x <- (reverse (helper n))])} 

//Start = f 4 // {1,1,2,1,2,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1}
//Start = f 3 // {1,1,2,3,2,3,3,3,3,3,3,3}

:: Tree a = Node a (Tree a) (Tree a)|Leaf

// Compute the average of all the numbers placed in the nodes of a tree.
treetolist :: (Tree Int) -> [Int]
treetolist Leaf = []
treetolist (Node x l r) = [x] ++ (treetolist l) ++ (treetolist r)


avg :: (Tree Int) -> Real
avg Leaf = 0.0
avg  x = toReal(sum (treetolist x))/toReal(length(treetolist x))



//Start = avg (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 6 (Node 4 Leaf (Node 4 Leaf Leaf)) (Node 1 Leaf Leaf))) // 3.0
//Start = avg Leaf // 0
