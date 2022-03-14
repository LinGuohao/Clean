module ex61
import StdEnv
//1. Map a list of functions to a value. E.g. mapfun [f,g,h] x = [f x, g x, h x]
/*mapfun :: [Int -> Int] Int -> [Int]
mapfun l v = map(\x = x v) l


Start = mapfun [inc, inc, inc] 3 // [4, 4, 4]
*/

// 2. compute n! factorial using foldr
/*f:: Int -> Int
f x =  foldr  (*) 1 (list x)

list:: Int -> [Int]
list 0 = []
list x = [x] ++ list(x - 1)

Start = f 5 // 120
*/
// 3. rewrite flatten using foldr 
// (for the following list [[1,2], [3, 4, 5], [6, 7]] => [1,2,3,4,5,6,7])
/*flat:: [[Int]] -> [Int]
flat [] =[]
flat [a:b] = flatten [a] ++ flat b 


Start = flat [[1,2], [3, 4, 5], [6, 7]] // [1,2,3,4,5,6,7]

// 4. using map and foldr compute how many elements are altogether in the following list 
// [[1,2], [3, 4, 5], [6, 7]] => 7
g :: [[Int]] -> Int
g n =  foldr (+) 0 (map(\x = length x) n)



Start = g [[1,2], [3, 4, 5], [6, 7]] // 7

// 5. using map extract only the first elements of the sublists in 
// [[1,2], [3, 4, 5], [6, 7]] => [1,3,6]
firsts :: [[Int]] ->[Int]
firsts [] = []
firsts n = map hd  n


Start = firsts [[1,2], [3, 4, 5], [6, 7]] // [1,3,6]


// 6. compute the squares of the elements of a list using map
// [1, 2, 3] -> [1, 4, 9]
sqrs :: [Int] -> [Int]
sqrs [] = []
sqrs n = map (\x = x^2) n



Start = sqrs [1, 2, 3] // [1, 4, 9]
*/
// 7. same as 6. with lambda expression




Start = sqrs_lambda [1, 2, 3] // [1, 4, 9]
