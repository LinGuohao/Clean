module cw04g2 
import StdEnv

// 1. For every sublist, eliminates its elements // Until the current element is a power of 2 // Requirement: // - Use map instead of recursion // 
f1 :: [[Int]] -> [[Int]]
f1 x = map(\x = dropwhile ispower x) x
ispower :: Int -> Bool
ispower 1 = True
ispower 2 = True
ispower 0 = False
ispower x
| isOdd x = False
= ispower (x/2)





Start = f1 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[1,2,3,4], [4,3,0], [], [], [128, 64, 32]] // Start = f1 [[1], [4], [2]] // [[1], [4], [2]] // Start = f1 [[5..10], map (= x + 5) [1..4], [], [5, 4]] // [[8, 9, 10], [8, 9], [], [4]]

// 2. Eliminates all numbers whose absolute value is a power of 2 in the list // f2 :: [Int] -> [Int]

// Start = f2 [1, -2, 3, 5, 7, -9, 11] // [3, 5, 7, -9, 11] // Start = f2 [1, 9, 2, 4, 7, 5, 1024, 2047] // [9, 7, 5, 2047] // 

fun x =  not(ispower (abs x))

f2 :: [Int] -> [Int]
f2 a = filter fun a

ispower :: Int -> Bool
ispower 1 = True
ispower 2 = True
ispower 0 = False
ispower x
| isOdd x = False
= ispower (x/2)

Start = f2 [1, -2, 3, 5, 7, -9, 11]




//Start = f2 [] // []



// 3. Create a list contains all third elements of the input // Requirement: If you finish this without map/foldr, you will still have points, just not full points // - Use map // - Use foldr

 f3 :: [[Int]] -> [Int]


f3 [x:y:z:t] = map get3 [f3 [x:y:z]]
 get3 :: [Int] ->[Int]
get3 [x:y:z] = [z]
 
 
 
 

Start = f3 [[], [1, 2, 3], [2, 1, 3], [6..9]] // [3, 3, 8] // Start = f3 [[], []] // []




