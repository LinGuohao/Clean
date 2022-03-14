 module test3
import StdEnv

/*
Write a function that converts binary numbers to decimal numbers.

For example: 10010 = 2^4 + 2^1 = 18

//binaryToDecimal :: Int -> Int 0010
//Start = binaryToDecimal 10010 //18
//Start = binaryToDecimal 1010101010101 //5461
*/
binaryToDecimal :: Int -> Int
binaryToDecimal n
|n == 0 = 0
= sum1 (list n)  



list :: Int -> [Int]
list n
| n == 0 = []
= [n rem 10] ++ list (n/10) 

//Start = list 10010

sum1 :: [Int] -> Int
sum1 n 
|n == [1] = 1 
|n== [] = 0
|last n == 1 = 2^((length n) - 1) + sum1 (init n)
=  sum1 (init n)
 


Start = binaryToDecimal 10

/*|n== [1] = 1 
|n== [] = 0
|last n == 1 = 2^((length n) - 1) + sum1 (init n)
=  sum1 (init n)
 */
