module test1
import StdEnv


/**
  * Write a function that takes a list of coefficients for a polynomial
  * and evaluates it at an integer given as the second parameter.
  * A list such as [1,6,9] would represent the the polynomial x^2+6x+9.
  * Note: Exponentiation via (^) or a custom exponential function
  * is NOT allowed.
  * 
  * For example: Evaluate [1,6,9] 2 = 25
  * Hint: Use Horner's Method
  * e.g. 3x^2 + 2x -4 = -4 + 2x + 3x^2 = -4 + x(2 + x(3)))
  *  3x^2 + 2x -4 = -4 + x * (3x +2)
  * [3,2,-4]                   [3,2]
  * Total: 50pts
  */
//Evaluate :: [Int] Int -> Int
//Evaluate [] x = 0
//Evaluate [a] x = a
//Evaluate [a:b] x = (last [a:b]) + x * (Evaluate (init [a:b]) x)
//Start = Evaluate [1,6,9] 2 //25
//Start = Evaluate [1337] 12345 //1337
//Start = Evaluate [] 9001 //0
//Start = Evaluate [243,810,1080,720,240,32] (~2) //-1024

Evaluate :: [Int] Int -> Int
Evaluate [] x =0
Evaluate [a] x = a
Evaluate [a:b] x = a* (helper [a:b] x) + Evaluate b x


helper :: [Int] Int -> Int
helper [] x = 0
helper [a] x = 1
helper [a:b] x = x* helper b x 
               
Start = Evaluate [243,810,1080,720,240,32] (~2) //-1024
