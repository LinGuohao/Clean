module test3
import StdEnv

// 1. Takes out every 3rd element from the list f1 :: [Int] -> [Int]
f1:: [Int] -> [Int]
f1 [] = []
f1 a = take 2 a ++ f1(drop 3 a)





Start =  f1 [1, 2, 3, 4, 5, 6, 7, 8] // [1, 2, 4, 5, 7, 8]

// 2. Concatenates all 2nd sublists into a list f2 :: [[Int]] -> [Int]
f2 :: [[Int]] -> [Int]
f2 [] = []

f2 [_,x:y] = x ++ f2 y





Start = f2 [[2, 1, 3], [], [1, 5, 2], [1, 2, 3], [0, 0], [1]] // [1, 2, 3, 1]
 /*
// 3. For every sublist, eliminates its elements // Until the current element is a power of 2 f3 :: [[Int]] -> [[Int]]






// Start = f3 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[1,2,3,4], [4,3,0], [], [], [128, 64, 32]]