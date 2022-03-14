module ex22
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf
// Define a type Tree for representing binary trees.
// 1. Write a function that returns the maximum value of all the values stored 
// in a binary tree. Assume all values are positive; return -1 if the tree is empty.
f1 :: (Tree Int) -> Int
f1 Leaf = 0
f1 (Node x l r) = max  (max x (f1 l)) (f1 r) 



//Start = f1 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 6 (Node 4 Leaf (Node 5 Leaf Leaf)) (Node 1 Leaf Leaf)))


// 2. Compute the average of all the numbers placed in the nodes of a tree.
many :: (Tree Int) -> Int
many Leaf = 0
many (Node x l f) = 1 + many l + many f


add :: (Tree Int) -> Int
add Leaf = 0
add (Node x l r) = x + add l + add r 

f2 :: (Tree Int) -> Real
f2 x = (toReal(add x)) / (toReal(many x))   


//Start = f2 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 6 (Node 4 Leaf (Node 5 Leaf Leaf)) (Node 1 Leaf Leaf)))


// 3. Build a search tree form a list then insert a number in the tree.
f3 :: a [a] -> (Tree a) | Ord, Eq a
f3 x list = f3helper2 x (f3helper list)

f3helper2 :: a (Tree a) -> (Tree a) | Ord, Eq a
f3helper2 n Leaf = (Node n Leaf Leaf)
f3helper2 n (Node x l r)
| n == x = (Node x l r)
| n < x = (Node x (f3helper2 n l) r)
| n > x = (Node x l (f3helper2 n r))



f3helper :: [a] -> (Tree a) | Ord ,Eq a
f3helper list
| isEmpty list = Leaf
= (Node (sorted!!mid) (f3helper (take mid sorted)) (f3helper (drop (mid+1) sorted)))
	where
		sorted = sort (removeDup list)
		mid = (length sorted)/2 

//Start = f3 13 [1..10]


////////

:: Q = {nom :: Int , denom :: Int}
// Define a type Q for rational numbers.
// 4. Write a function to test the equality of rational numbers.



f4 :: Q Q -> Bool
f4 a b 
|(a.nom == b.nom) && (a.denom == b.denom) = True
=False
//Start = f4 {nom = 12, denom = 6} {nom = 39, denom = 18}


// 5. Add two rational numbers.
f5 :: Q Q -> Q
f5 x y = {nom= x.nom + y.nom , denom = x.denom + y.denom} 


//Start = f5 {nom = 3, denom = 4} {nom = 3, denom = -3}


// 6. Given two rational numbers multiply them.
//f6 :: Q Q -> Q

//Start = f6 {nom = 3, denom = 4} {nom = 3, denom = -3}


////////
// 7. Given 4 points of type Point (with the real fields x and y), check if they form a rectangle.
//f7 :: [Point] -> Bool


//Start = f7 [{x = 1.0, y = 1.0}, {x = 5.0, y = 5.0}, {x = 1.0, y = 5.0}, {x = 5.0, y = 1.0}]


////////
// 8. Generate an array that has as elements 1,2,2,3,3,3,4,4,4,4,...,10,..,10.

f8 :: {Int}
f8 =  { x \\x <-(flatten [repeatn z z\\z <- [1..10] ])}


//Start = f8


// 9. For a given n and i generate an array like
// for n=4 and i=1 the result is {1,1,2,1,2,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1}
f9 :: Int Int -> [Int]
f9 x m = flatten[(f9helper)((repeatn z z)) m \\ z <- [1..4]]


f9helper :: [Int] Int -> [Int]
f9helper [] _ = []
f9helper x y = flatten [[z] ++ [y]\\ z <- x]

//Start = f9 4 1


////////
// 10. Is x a power of 2? 
f10 :: Int -> Bool
f10  x 
|x == 2 = True
|(x rem 2) <> 0 = False
|(x rem 2 == 0 ) = f10 (x / 2)


//Start = f10 10
//Start = f10 32


// 11. Delete from a list all the numbers that are prime.
f11 :: [Int] -> [Int]
f11 x = [z\\ z <- x | f11helper z]

f11helper :: Int -> Bool
f11helper  z
|z == 0 = False
|z == 1 = False
= [x \\ x  <- [2..z/2] | z rem x == 0] == [] 

//Start = f11 [1,3,4,6,9,17,8,10,21,23,41,100]


// 12. Compute the factorial of a number x using foldr.
f12 :: Int -> Int
f12 x = foldr (*) 1 [1..5]
//Start = f12 5


// 13. Add the last number of a list to every element before that (the list has min 2 elements)
f13 :: [Int] -> [Int]
f13 x =  [foldr (+) 0 (take ((length x) - 1) x)] ++ [foldr (+) 0 x] 
//Start = f13 [1, 3, 4, 2, 10]


// 14. Check in a list if every second element of a list is even.
//   [1,1,1,2,11,4] -> 
f14 :: [Int] -> Bool
f14 [] = True
f14 x = f14helper2 (f14helper x)

f14helper :: [Int] -> [Int]
f14helper [] = []
f14helper x 
|length x == 1 = []
= take 1 (drop 1 x) ++  f14helper (drop 2 x)

f14helper2 :: [Int] -> Bool
f14helper2 x 
|length [z\\z <- x |isEven z]  == length x = True
= False

//Start = f14 [1,1,1,2,11,4] // False
//Start = f14 [1,12,3,44,5] // True
//Start = f14 [] // True 


// 15. Generate the first 100 positive integer elements that are not prime.
f15 :: [Int]
f15 = [z\\z <- [1..100] | f11helper z]
Start = f15