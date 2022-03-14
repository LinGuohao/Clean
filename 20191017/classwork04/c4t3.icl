module c4t3
import StdEnv
// 3. Create a list contains all third elements of the input 
// Requirement: If you finish this without map/foldr, you will still have points, just not full points
// - Use map
// - Use flatten
f3 :: [[Int]] -> [Int]
f3 [] = []
f3 n = flatten (map (\x = take 1 (drop 2 x)  ) n) 






Start = f3 [[], [1, 2, 3], [2, 1, 3], [6..9]] // [3, 3, 8] // Start = f3 [[], []] // []