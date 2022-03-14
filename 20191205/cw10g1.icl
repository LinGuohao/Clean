module cw10g1
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf

/*
Write a function returning the depth
of the largest prime number in the tree.
*/
isPrime :: Int -> Bool
isPrime x = and[x rem n <> 0\\n <- [2..(x-1)]]


f1:: (Tree Int)  Int -> [(Int,Int)]
f1 Leaf _ = [(0,0)]
f1 (Node x l r) y 
|isPrime x = [(x,y)] ++ f1 l (y + 1) ++ f1 r (y + 1)
= [(0,y)] ++ f1 l (y + 1) ++ f1 r (y + 1)

sort1 :: [(Int,Int)] -> Int
sort1 x = snd (last (sort[z\\ z <- x]))


deepPrime :: (Tree Int) -> Int
deepPrime Leaf = 0
deepPrime x =  sort1 (f1 x 0)



//Start =  deepPrime ourTree //3
//Start = deepPrime tree1 //1
//Start = deepPrime Leaf //0

/*
Write a class 'lol'
with instances for Int, Real, Bool, String, and [a].
For Int, return that number + 1.
For Real, return the square root of that number.
For Bool, return the opposite boolean.
For String, return the String in reverse.
For a list [a], return the list concatenated to itself.
*/
class lol a :: a -> a
instance lol Int
where 
         lol  a = a + 1
instance lol Real
where
         lol a = sqrt a       


instance lol String
where
         lol a =  {m\\m<-reverse[z\\ z <-: a]}
         
instance lol [a]
where
         lol a = a ++ a

instance lol Bool
where     
          lol True = False
          lol False = True
//instance lol Int
//instance lol Real
//instance lol Bool
//instance lol String
//instance lol [a]
//Start = lol 41 //42
//Start = lol 176752.9764 //420.42
//Start = lol True //False
//Start = lol "partyboob" //"boobytrap"
//Start = lol [1,2,3,4] //[1,2,3,4,1,2,3,4]

/*
Given an array, extract the elements
located at the prime numbered index locations
and return them in a new array.
*/
//primeIndex :: (a e) -> (a e) | Array a e
/*Start :: {Int}
Start = primeIndex {x\\x<-[1..100]}*/
//{1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97}
//Start = primeIndex "Helplooblo aWes omrporlyeet!d !"