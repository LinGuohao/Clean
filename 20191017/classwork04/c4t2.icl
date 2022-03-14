module c4t2 
import StdEnv
//1. For every sublist, eliminates its elements 
// Until the current element is a power of 
// Requirement: // - Use map instead of recursion // 
f1:: [[Int]] -> [[Int]]
f1 [] = []
f1 [a:b] = map (\x = dropWhile (not o ispower ) x) [a:b]


ispower :: Int -> Bool
ispower 1 = True
ispower 2 = True
ispower 0 = False
ispower x
| isOdd x = False
= ispower (x/2)




Start = f1 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[1,2,3,4], [4,3,0], [], [], [128, 64, 32]]
 // Start = f1 [[1], [4], [2]] // [[1], [4], [2]] 
 // Start = f1 [[5..10], map (= x + 5) [1..4], [], [5, 4]] // [[8, 9, 10], [8, 9], [], [4]]