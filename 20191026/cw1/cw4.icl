module cw4
import StdEnv

// 1.Use foldr to check if every element in a list is a power of n?
/*
f1::Int [Int]-> Bool
f1 x n 
|n == [] = True
= ispower x (foldr (*)1 n)


ispower :: Int Int -> Bool
ispower x y
|y rem x <> 0 = False
|y == 1 = True
|y > 1 = ispower x (y/x)
= False


*/
//Start = f1 2 [1,2,4,8]  //True

//Start = f1 2 [1,2,4,80] //False

//Start = f1 9 [] //True


// 2.Use list comprehension to generate the list [1,2,2,4,4,4,4,8,8,8,8,8,8,8,8,16...16,32...32]
f2 :: [Int] -> [Int]
f2 n = flatten [repeatn n n\\ n <- [1..32]| isEven 2 ]

ispower :: Int Int -> Bool
ispower x y
|y rem x <> 0 = False
|y == 1 = True
|y > 1 = ispower x (y/x)
= False


Start = f2 [1]


//3. filter a list of  triple tuples which can form acute triangle 
//(An acute triangle is a triangle with three acute angles (less than 90бу). ) 
//eg. [(3,3,3),(3,4,5),(1,0,2),(3,~3,3),(5,5,8)] -> [(3,3,3)] */

acute::(Int,Int,Int) ->Bool
acute (a,b,c)  
|((a^2 + b^2) > c^2) && a>0 && b>0 && c>0  = True
= False



f3::[(Int,Int,Int)] -> [(Int,Int,Int)]
f3 n = [x\\ x <- n | acute x ]



Start = f3 [(3,3,3),(3,4,5),(1,0,2),(3,~3,3),(5,5,8)]  //[(3,3,3)]
*/