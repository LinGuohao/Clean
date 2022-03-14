module test2 
import StdEnv


//1. For every sublist, compute the tuple (difference of first and last, difference of last and first) // Use map!
f :: [Int] -> (Int,Int)
f [] = (0,0)
f [a]= (0,0)
f n = (last n - hd n , hd n - last n)
sums :: [[Int]] -> [(Int,Int)]
sums n

= map (\ x = f x) n


Start = sums [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [(1, -1), (2, -2), (1, -1), (0, 0), (0, 0)]