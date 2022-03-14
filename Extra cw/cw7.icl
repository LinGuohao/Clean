module cw7
import StdEnv

/*1. generate function as following: 
we pick out the element who will get maximum result 
after applied the function(second parameter).
[1,2,3,4,5] ((*) (~1))  -> 1
[1,2,3,4,5] ((*) 2)  -> 5
 if we have a function : f x = (-1)*(3-x)^2
then : [1,2,3,4,5] f  -> 3
*/


f1h :: [Int] Int (Int -> Int)-> [(Int,Int)]
f1h [] _ _ = []
f1h [x:xs] y f = [((f x),y)] ++ f1h xs (y + 1) f 

f1 :: [Int] (Int->Int) -> Int
f1  x f =   hd [a\\(a,b)<-(f1h x 0 non) | b == snd ( last (sort(f1h x 0 f)))] 

where non = ((*)1)

//Start = f1 [1,2,3,4,5] ((*) (~1))  //1
 
//Start = f1 [1,2,3,4,5] ((*) 2)  //5

//f x = (-1)*(3-x)^2

//Start = f1 [1,2,3,4,5] f //3  


/*2. Given a list and a number n,
we group the list to sublist in every n number.
e.g. [1,2,3,4,5] 2 -> [[1,2],[3,4],[5]]
*/

f2::[Int] Int -> [[Int]]
f2 x n 
|length x <= n = [x]
= [take n x] ++ (f2 (drop n x) n)



//Start = f2 [1,2,3,4,5] 2 //[[1,2],[3,4],[5]]

//Start = f2 [1..10] 6 //[[1,2,3,4,5,6],[7,8,9,10]]


/*
3. find the avege value of the nodes in a tree
*/

:: Tree a = Node a (Tree a) (Tree a)
          | Leaf

atree = Node 6 (Node 3 (Node 6 Leaf Leaf) Leaf) Leaf




f3::(Tree Int) -> Int
f3 Leaf = 0
f3 x = sum (tolist x) /length(tolist x)

where tolist Leaf = []
      tolist (Node x l r) = [x] ++ tolist l ++ tolist r 


//Start = f3 Leaf  //0

Start = f3 atree //5