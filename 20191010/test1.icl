module test1
import StdEnv

/*
Write a function that takes a list of numbers and
breaks it into two lists with alternating members from
the original list.

For example: [3,5,6,8,7,9] -> [ [3,6,7], [5,8,9] ]
*/
//splitList :: [Int] -> [[Int]]
//Start = splitList [56,3,87,5,234,5,0,-4] //[[56,87,234,0],[3,5,5,-4]]
//Start = splitList [1,4..50] //[[1,7,13,19,25,31,37,43,49],[4,10,16,22,28,34,40,46]]
//Start = splitList [420] //[[420],[]]
//Start = splitList []//[[],[]]

splitlist :: [Int] -> [[Int]]
splitlist [] = [[],[]]
splitlist [a] = [[a],[]]
splitlist [a:b] = [list1 [a:b]] ++ [list2 [a:b]] 
 

list1 :: [Int] -> [Int]
list1 [] = []
list1 [a] = [a]
list1 [a:b] =  [a] ++ list1 (drop 2 [a:b])

list2 :: [Int] -> [Int]
list2 [] = []
list2 [a:b] = take 1 b ++ list2 (drop 1 b) 

Start = splitlist [1,4..50]