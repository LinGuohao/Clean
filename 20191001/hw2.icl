/**
  * Write a function that takes an integer 'x'
  * and returns a boolean corresponding to whether or not x is prime.
  * A prime number is a number that has only itself and 1 as divisors.
  * 1 is not prime.
  *
  * Total: 30pts
  */
//isPrime :: Int -> Bool
//Start = isPrime 5 //True
//Start = isPrime (~3) //False (Negative numbers don't count, only Natural numbers)
//Start = isPrime 0 //False
//Start = isPrime 1 //False
//Start = isPrime 28736 //False
module hw2
import StdEnv


isPrime ::  Int-> Bool
isPrime n
|n<=1 =False
| n == 2 = True
= helper n (n-1)

helper :: Int Int -> Bool
helper n x
|x == 1 = True 
|n rem x <>0 =  helper n (x-1)
|n rem x ==0 = False
Start = isPrime -11
