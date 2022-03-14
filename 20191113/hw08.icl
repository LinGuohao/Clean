module hw08
import StdEnv 

tolist :: (Tree a) -> [a]
tolist Leaf = []
tolist (Node a b c) = [a] ++ tolist b ++ tolist c

/*
Write an instance of equality (==) between two Trees,
such that we can use calls such as treeA == treeB.
*/



instance == (Tree a) | Eq a
where
    == tree1 tree2 = tolist tree1 == tolist tree2 
//Start = ourTree == ourTree //True
//Start = ourTree == oddTree //False
//Start = isMember ourTree [ourTree, oddTree, Leaf] //True

/*
Write a function that will take the data constructs
and apply the array of conditions in the array portion,
and apply it with the boolean aggregator function (first portion)
to every element of the tree (second portion),
returning a list of elements that return True after processing.

For example:
Given a data construct of Core or someTree {cond1,cond2,cond3,cond4},
then return a list of elements from someTree, given that they return
True for one or more (the 'or' function) of the conditions from the
array (cond1, cond2, etc...).

Hint: Arrays are easier handled as lists for some operations.
*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf
:: Yggdrasil a b c = Core a (Tree b) {c}

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

oddTree :: (Tree Int)
oddTree = (Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 9 Leaf Leaf) Leaf))

flow1 :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool))
flow1 = Core and ourTree {isEven, ((<)10)}


flow2 :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool))
flow2 = Core or oddTree {((<)10),((==)1),(\x = and[x rem n <> 0\\n<-[2..(x-1)]])}







flowApp :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool)) -> [Int]
flowApp (Core a b c) = f2 (tolist b) c a


f2 :: [Int] {Int -> Bool} ([Bool] -> Bool) -> [Int]    
f2 [] _ _ = []
f2 a b c
|c [f (hd a) \\ f <-: b] == True = [hd a] ++ f2 (tl a) b c
= f2 (tl a) b c  



//Start = flowApp flow1 //[18,20,24,26,28]
//Start = flowApp flow2 //[1,3,5,7,11]

/*
Write a function that checks if a String is a palindrome or not.

Hint: Strings are just arrays. ;)
*/

compare :: [Char][Char] -> Bool
compare [] [] = True
compare x y 
|hd x == hd y = compare (tl x) (tl y)
=False




palindrome :: String -> Bool
palindrome x = compare (reverse [z\\ z <-: x])([m\\ m <-: x])


//Start = palindrome "racecar" //True
//Start = palindrome "amanaplanacanalpanama" //True
//Start = palindrome "boobytrap" //False
