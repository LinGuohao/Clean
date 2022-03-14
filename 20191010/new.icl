module new
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

FibList :: Int ->[Int]
FibList n
|n<0 = []
|n ==0 = []
|n ==1 =[1]
|n ==2 =[1]
= FibList(n-2) ++ FibList(n-1)
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
Start = FibList 8 //[1,1,2,3,5,8,13,21]
//Start = FibList -3 //[]
//Start = FibList 0 //[]