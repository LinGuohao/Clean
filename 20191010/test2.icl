module test2
import StdEnv

/*
Write a function that takes a list of numbers and
adds the first element, subtracts the second element,
adds the third element, subtracts the fourth element,
in this alternating repetition.

For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3
*/
//alternatingSum :: [Int] -> Int
//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
//Start = alternatingSum [] //0

alternatingSum :: [Int] -> Int
alternatingSum [] = 0
alternatingSum [a:b] = list1 [a:b] - list2 b

list1 :: [Int] -> Int
list1 [] = 0
list1 [a:b] = hd [a:b] + list1 (drop 2 [a:b])

list2 :: [Int] -> Int
list2 [] = 0
list2 b = hd b + list2 (drop 2 b)

Start = alternatingSum [45,-5,63,46,-345,4321]



