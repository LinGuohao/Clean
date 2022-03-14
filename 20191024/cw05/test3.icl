module test3 
import StdEnv
// 3. Generate a square matrix from the given size (index start from 1) 
// where the lower triangle are zeros 
// and for the upper triangle (start from the main diagonal): (a!!i)!!j = j - i. 
// Bonus: // - Can you flip the matrix horizontally? 
// - What if we provide an array with size n as a parameter (b), and we want (a!!i)!!j = b!!i - j? 
Matrix4 :: Int -> [[Int]]
Matrix4 n = [(take n (iterate ((+)1) 0))] ++  f (n) (n-1) ++ [repeatn n 0]

f :: Int Int  -> [[Int]] 
f x y
|y == 1 = []
=  reverse (([take x ((repeatn (y - 1) 0 ++ (take x (iterate ((+)1) 0))))]) ++ reverse (f x (y-1)))




Start = Matrix4 6  // [[0,1,2],[0,0,1],[0,0,0]] // Start = Matrix4 4 // [[0,1,2,3],[0,0,1,2],[0,0,0,1],[0,0,0,0]]