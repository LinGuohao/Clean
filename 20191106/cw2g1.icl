module cw2g1
import StdEnv

// 1. Compute the sum of the list excluding the largest and the smallest number
f1 :: [Int] -> Int 
f1 n = sum (drop 1 (init(sort n)))



//Start = f1 [1..5] //  9
//Start = f1 [4,6,2,7,1,9] //  19
//Start = f1 [-4,6,2,7,1,9] // 16

// 2. Replace every second number from the list by 1,2,3...
f2 :: [Int] -> [Int]
f2 [] = []
f2 [n] = [n]
f2 [a:b] = f3 [a:b] 1

f3 :: [Int] Int -> [Int]
f3 [] m = []
f3 [a:b] m = [a] ++ [m] ++ f3 (drop 2 [a:b] )(m + 1) 

//Start = f2 [] //[]
//Start = f2 [4] //[4]
//Start = f2 [6,4] //[6,1]
//Start = f2 [6,6,6,6] //[6,1,6,2]
//Start =  f2 [1,9,0,2,4,7,0,5,1,8,3,0,1,2] // [1,1,0,2,4,3,0,4,1,5,3,6,1,7]

// 3. Divide a list of integer into two lists
// One containing even numbers and one containing odd numbers
//[1,2,3,4,5,6] -> [[2,4,6],[1,3,5]]
f4::[Int] -> [[Int]]
f4 n = [[x\\x <- n |isEven x]] ++ [[x\\x <- n |isOdd x]]

//Start = f4 [1..6] //[[2,4,6],[1,3,5]]
Start = f4 [34..43]  //[[34,36,38,40,42],[35,37,39,41,43]]