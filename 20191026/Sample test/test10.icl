module test10
import StdEnv



/**10 Insert sum of elements as last element in every sublist of a list. */

addSum :: [[Int]] -> [[Int]]
addSum n = map (\x = x ++ [sum x]) n

Start = addSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]