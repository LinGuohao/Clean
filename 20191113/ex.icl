module ex
import StdEnv


//thd3 (a,b,c) = c
sortTriple :: [(Int,Int,Int)] -> [(Int,Int,Int)]
sortTriple [] = []
//sortTriple [x:xs] = sortTriple [a\\ a <- xs | thd3 a < x ] ++ [x] ++  sortTriple [a\\ a <- xs | thd3 a >= x ]

//Start = [(1,2,3),(4,5,1)] //-> [(4,5,1),(1,2,3)]


:: Person = {name:: String , age :: Int}

extractAge :: Person -> Int
extractAge x = x.age

Start = map extractAge [{name = "hossam" , age = 19} , {name = "nikola" ,age = 20}]