module test7
import StdEnv



/*7
// Given a list of lists, for each list, extract the first, middle and last element. 
*/

//Points3 :: [[Int]] -> [(Int, Int, Int)]
//points3 n =  3 4 5 6 7 8 9 10 11

f :: [[Int]] -> [(Int,Int,Int)]
f [] = []
f [x:xs] = [(hd (x),  x !! ((length x)/2)    ,last (x))] ++ f xs




Start = f [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

//Start = Points3 [[]] //[]
