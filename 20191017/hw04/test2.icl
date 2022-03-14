module test2
import StdEnv
/**
  * 30 pts
  * Write a function that takes a list of integers and reverses their digits and order.
  * 
  * For example: ReverseDig [123,456,789] = [987,654,321]
  */
//ReverseDig :: [Int] -> [Int]
//Start = ReverseDig [123,456,789] //[987,654,321]
//Start = ReverseDig [] //[]
//Start = ReverseDig [1,23,456,7891,23456] //[65432,1987,654,32,1]

ReverseDig :: [Int] -> [Int]
ReverseDig [] = []
ReverseDig n = reverse (Reverse n)



Reverse::[Int]->[Int]
Reverse [] = []
Reverse [a:b] =  [(reint (list a))] ++ (Reverse b)

list :: Int -> [Int]
list a
| a == 0 = []
= [a rem 10] ++ list (a/10)

reint:: [Int] -> Int
reint [] = 0
reint [c] = c
reint [c:d] = c * 10 ^((length [c:d]) - 1) + reint d 




Start =  ReverseDig [1,23,456,7891,23456]