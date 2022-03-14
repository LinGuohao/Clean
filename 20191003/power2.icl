module power2
import StdEnv



//3. For every sublist, eliminates its elements // Until the current element is a power of 2 f3 :: [[Int]] -> [[Int]]

// Start = f3 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[1,2,3,4], [4,3,0], [], [], [128, 64, 32]]


f3 :: [[Int]] -> [[Int]]
f3 [] = []
f3 [[]:b] = f3 b
f3 [a:b] = [check a] ++ f3 b






check :: [Int] -> [Int]
check [] = []
check a = dropWhile  (not o ispower)  a // (\x = not (ispower x)) [x:xs] 




ispower :: Int -> Bool
ispower 1 = True
ispower 2 = True
ispower 0 = False
ispower x
| isOdd x = False
= ispower (x/2)

Start = f3 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]]