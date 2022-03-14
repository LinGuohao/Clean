module C3JI5D_Homework09
import StdEnv

// 1. Given a list of triple tuples compute for each tuple (x,y,z) the value (2x,y/2,2z+1)
func1 :: [ ( Int, Int, Int ) ] -> [ ( Int, Int, Int ) ]
func1  x = [(2 * a , b / 2 , 2 * c + 1)\\ (a,b,c)<- x]
// 2. Generate the first 10 element of list like: [[0],[1,1],[0,1,2,2,1,0],[0,1,2,3,3,2,1,0] ...]
//func2 = [[0], [1,1]] ++ (take 8 [ [0..x] ++ (reverse[0..x]) \\ x <- [2..] ])

// 3. Generate the first 10 powers of 2 [1,2,4,8,16 ,...]

//func3 = take 10 [ 2^x \\ x <- [0..] ]
func3 = take 10 [2^x   \\x  <- [0..]  ]
//Start = func1 [(1,2,3), (2,3,4)]
//Start = func2
Start = func3