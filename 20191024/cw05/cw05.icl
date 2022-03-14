module cw05
import StdEnv

// 1. For every sublist, compute the tuple (difference of first and last, difference of last and first)
 // Use map!
f :: [Int] -> (Int,Int)
f [] = (0,0)
f [a]= (0,0)
f n = (last n - hd n , hd n - last n)
sums :: [[Int]] -> [(Int,Int)]
sums n

= map (\ x = f x) n


Start = sums [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [(1, -1), (2, -2), (1, -1), (0, 0), (0, 0)]



//sums :: [[Int]] -> [(Int, Int)] //Start = sums [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [(1, -1), (2, -2), (1, -1), (0, 0), (0, 0)]

// 2. Generate a list of leap year with the maximum element smaller than a given Integer 
// How to find leap year? // if (year is not divisible by 4) then (it is a common year) 
// else if (year is not divisible by 100) then (it is a leap year) 
// else if (year is not divisible by 400) then (it is a common year) // else (it is a leap year)

list :: Int ->[Int]
list x = takeWhile ((>)x) (iterate ((+)4)  4)
LeapYears :: [Int] -> [Int]
LeapYears [h:t] = [h\\ h <- [h:t] |(h rem 100) <> 0 || (h rem 400) == 0]
LeapYears1 :: Int -> [Int]
LeapYears1 x = (LeapYears (list x)) 
//Start = LeapYears1  400


//Start = LeapYears 5 // [4] 
//Start = LeapYears1 101 // [4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96]

// Start = LeapYears 5 // [4] // Start = LeapYears 101 // [4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96]


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




//Start = Matrix4 6  // [[0,1,2],[0,0,1],[0,0,0]] // Start = Matrix4 4 // [[0,1,2,3],[0,0,1,2],[0,0,0,1],[0,0,0,0]]