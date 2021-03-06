module cw6
import StdEnv

/* 1. Compute the average of the elements of the sublists of a list 
*/

f1 :: [[Int]] -> [Int]


f1  x = [sum z / length z\\z <- x]

//Start = f1 [[1, 2, 3], [3, 4], [5, 7, 8, 9], []] //[2,3,7,0]



/* 2. Generate pairs like in the following: (No.,avg,sum)
[1,2,3], [4,5], [6,1,8,9], []] -> [(1,2,6),(2,4,9),(3,6,24),(4,0,0)]
*/

//f2 :: [[Int]] -> [(Int, Int,Int)]

//Start = f2 [[1,2,3],[4,5],[6,1,8,9],[]] //[(1,2,6),(2,4,9),(3,6,24),(4,0,0)]

/*3. Generate a list of list as the following:
[1,2,3,4,5,6,5,4,3,2,1] 4 -> [[1,2,3],[5,6,5],[3,2,1]]
you only need to find the first two matching numbers
*/

f3::[Int] Int -> [[Int]]
f3 m x = f3h m x 0


f3h ::[Int] Int Int -> [[Int]]
f3h [] _ _ = []
f3h m x y 
|y<=1 = [fst (span ((<>)x) m)] ++ f3h (drop 1 (snd (span ((<>)x) m))) x (y + 1)
= [m]



//Start = f3 [1,2,3,4,5,6,5,4,3,2,1] 4  //[[1,2,3],[5,6,5],[3,2,1]]

//Start = f3 [1,2,3,4,5,6,5] 4 //[[1,2,3],[5,6,5]]

Start = f3 [1,2,3,4,5,6,5,4,3,2,1,4,5,6,4] 4 //[[1,2,3],[5,6,5],[3,2,1,4,5,6,4]]