module test3
import StdEnv

/*
Given a list of tuples (a,b) -a,b integer numbers- such that
a and b are the sides  of a rectangle.
Return a list of triples (a,b,c) where c is the area of a rectangle
with dimensions a and b if and only if the area of that
rectangle is between 20 and 30*/

areaBetween20and30 :: [(Int,Int)] -> [(Int,Int,Int)]
areaBetween20and30 n =  f4 (list n)

list :: [(Int,Int)] -> [(Int,Int,Int)]
list n = map(\x = f x) n

f:: (Int,Int) -> (Int,Int,Int)
f (a,b) = (a,b,a*b)

f1 :: (Int,Int,Int) -> Bool
f1 (a,b,c) = c > 20 && c < 30

f4 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
f4  n = filter f1  n 


//Start = areaBetween20and30 [(1,34),(4,3),(5,5),(9,6),(3,9)]//[(5,5,25),(3,9,27)]
//Start=areaBetween20and30 [(5,6),(4,5),(20,1),(30,1)]//[]
//Start= areaBetween20and30 [(1,23),(7,3),(5,5),(7,6),(3,8),(12,2),(24,0),(0,0)]//[(1,23,23),(7,3,21),(5,5,25),(3,8,24),(12,2,24)]
