module hw1
import StdEnv


 // * Write a function that tests the Collatz conjecture.
  //* Given a number 'n', if it is even, divide it by 2.
  //* If 'n' is odd, multiply by 3 and add 1.
 // * Repeat until the number reaches 1.
 // * This function should return the total stopping time,
//  * which is the number of steps it took to reach 1.
//  *

F:: Int -> Int
F x
|x ==1 = 0
| isEven x = 1+ F(x/2)
| isOdd  x = 1 + F(x*3+1)

Start =  F 2