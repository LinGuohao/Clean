module test0
import StdEnv


/**
  * Write a function that takes a number 'n'
  * and returns a list of the first n Fibonacci numbers.
  * A Fibonacci number is a sequence where F_0 = 0, F_1 = 1, and F_n = F_(n-2) + F_(n-1)
  * For example, FibList 8 = [1,1,2,3,5,8,13,21]
  *
  * Note: You must use recursion.
  *
  * Total: 20pts
  */
//FibList :: Int -> [Int]
//Start = FibList 8 //[1,1,2,3,5,8,13,21]
//Start = FibList -3 //[]
//Start = FibList 0 //[]



List :: Int -> [Int]
List n
|n <=0 =[1]
|n == 1 = [1]
= List (n-1) ++ [Fiblist n]

Fiblist :: Int -> Int
Fiblist 1 = 1
Fiblist 2 = 1
Fiblist n= Fiblist (n-1)+ Fiblist (n-2)

Start = List 8 