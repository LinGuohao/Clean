module hw01
import StdEnv

/**
  * Write a function that tests the Collatz conjecture.
  * Given a number 'n', if it is even, divide it by 2.
  * If 'n' is odd, multiply by 3 and add 1.
  * Repeat until the number reaches 1.
  * This function should return the total stopping time,
  * which is the number of steps it took to reach 1.
  *
  * Total: 30pts
  */

//collatz :: Int -> Int
//Start = collatz 34 //13
//Start = collatz(collatz 10000001) //33
//Start = collatz 1 //0
//Start = collatz (~56) //0
  F:: Int -> Int
F x
|x ==1 = 0
| isEven x = 1+ F(x/2)
| isOdd  x = 1 + F(x*3+1)

Start =  F 2

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


/**
  * Write a function that takes an integer 'x'
  * and checks if this number is a palindrome.
  * A palindrome is a number or word that is identical 
  * when written forward or backwards.
  *
  * e.g. 1234 is not a palindrome. 145626541 is a palindrome.
  * Total: 40pts
  */
//isPalindrome :: Int -> Bool
//Start = isPalindrome 0 //True
//Start = isPalindrome 55 //True
//Start = isPalindrome 49594 //True
//Start = isPalindrome 1337 //False
//Start = isPalindrome (~57975) //False
isPalindrome :: Int -> Bool
isPalindrome n 
| n< 0 =False
|n <10 =True
= (intToList n) == reverse (intToList n)

intToList :: Int -> [Int]
intToList n 
| n < 10 = [n]
= intToList (n/10) ++ [n rem 10]

Start= isPalindrome 1234321