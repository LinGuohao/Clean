module test8
import StdEnv


/**8
//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] */
*/
f :: [(Int,Int,Int)] -> [Int]
f n = removeDup (sort (flatten ( map (\ (a,b,c) = [a,b,c]) n)))

f1 :: [Int] -> [(Int,Int,Int)]
f1 n = [(a,b,c)\\ a <- n , b <- n , c <- n | c^2 == (a^2 + b^2) && a > 0 && b > 0 && a < b]

f2 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
f2 n = f1 (f n)

//Start = f2 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

Start = f2 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]
