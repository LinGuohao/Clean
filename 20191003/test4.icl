module test4
import StdEnv

// 3. For every sublist, eliminates its elements // Until the current element is a power of 2 f3 :: [[Int]] -> [[Int]]

f3 :: [[Int]] -> [[Int]]
f3 [a:b] 
f3 [[]:b] = 0






// Start = f3 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[1,2,3,4], [4,3,0], [], [], [128, 64, 32]]
