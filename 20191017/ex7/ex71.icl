module ex71
import StdEnv


// Earlier exemples rewritten with map or fold.

// 1. Operations with lists: write functions for the followings
// keep the head of every sublist (sublist are not empty)
// e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
//heads :: [[Int]] -> [Int]
//heads n = foldr (++) [] n

//Start = heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]


// 2. Keep the tails of a list in 2 versions 
// e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [[2, 3], [4], [7, 8, 9]] 
//tails :: [[Int]] -> [[Int]]
//tails n = map tl n

//Start = tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]]


//3. Triple the elements op a list
//triples :: [Int] -> [Int]
//triples n = map (\x = 3*x) n

//Start = triples [1..5]


// 4. Check if the numbers of a list are odd.
//isoddnrs :: [Int] -> [Bool]
//isoddnrs n = map (isOdd) n

//Start = isoddnrs [1..5]


// 5. Add 100 to the numbers of a list.
//add100 :: [Int] -> [Int]
//add100 n = map (\x = x+100) n

//Start = add100 [1..8]


// 6. Check if the numbers of a list are multiple of 10.
//ismult10 :: [Int] -> [Bool]
//ismult10 n = map (\x = (x rem 10 == 0)) n

//Start = ismult10 [1..20]


// 7. Collect in a list the last digists of the numbers of a list.
//lastdigits :: [Int] -> [Int]
//lastdigits n = map (\x = x rem 10) n

//Start = lastdigits [1..35]


// 8. Compute the cube of the numbers of a list.
//cubes :: [Int] -> [Int]
//cubes n = map (\x = x^3) n

//Start = cubes [1..10]
//Start = cubes []

// 9.  Add the numbers from 1..N (N positive) using foldr.
//addn :: Int -> Int
//addn n = foldr (+) 0 [1..n]

//Start = addn 5
//Start = addn 0
//Start = addn -2
//Start = addn 10


// 10. Reverse every sublist of a list
//¢revsub :: [[Int]] ->  [[Int]]
//revsub n = map reverse n

//Start = revsub [[1,2,3],[5,6],[],[7,8,9,10]]


// 11. Keep the last elements of the sublists of a list in one list 
// (the sublists are not empty).
// [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
//lasts :: [[Int]] -> [Int]
//lasts n = map (\x = last x) n

//Start = lasts [[1,2,3],[5,6],[1],[7,8,9,10]]


// 12. Instert 0 in front of every sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is 
// [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
//ins0 :: [[Int]] -> [[Int]]
//ins0 n = map (\x = [0] ++ x) n

//Start = ins0 [[1,2,3],[5,6],[],[7,8,9,10]]


//13. Delete the last element of each sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[1,2],[5],[],[7,8,9]]
//lastdel :: [[Int]] -> [[Int]]
//lastdel n = map (\x = init x ) n

//Start = lastdel [[1,2,3],[5,6],[],[7,8,9,10]]


// 14. Compute the product of the elements of a list using foldr.
//prodf :: [Int] -> Int
//prodf n = foldr (*) 1 n

//Start = prodf [1,5,2,4]


// 15. Compute 1*1 + 2*2 + ... + n*n  for n positive using map and foldr.
sumsqr :: Int -> Int
sumsqr n = foldr (+) 0 (map (\x = x^2) (list n))


list :: Int -> [Int]
list 0 = []
list n = [n] ++ list (n-1)



Start = sumsqr 5 // 55