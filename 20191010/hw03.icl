module hw03
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


/*
Write a function that converts binary numbers to decimal numbers.

For example: 10010 = 2^4 + 2^1 = 18
*/
//binaryToDecimal :: Int -> Int
//Start = binaryToDecimal 10010 //18
//Start = binaryToDecimal 1010101010101 //5461
binaryToDecimal :: Int -> Int
binaryToDecimal n
|n == 0 = 0
= sum1 (list n)  



list :: Int -> [Int]
list n
| n == 0 = []
= [n rem 10] ++ list (n/10) 

//Start = list 10010

sum1 :: [Int] -> Int
sum1 n 
|n == [1] = 1 
|n== [] = 0
|last n == 1 = 2^((length n) - 1) + sum1 (init n)
=  sum1 (init n)
 


Start = binaryToDecimal 10

/*|n== [1] = 1 
|n== [] = 0
|last n == 1 = 2^((length n) - 1) + sum1 (init n)
=  sum1 (init n)
 */
