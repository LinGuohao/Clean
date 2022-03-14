module test2
import StdEnv

/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */

Listing :: [Int] -> [Int]
Listing [] = []
Listing n = f1 (n) (f n)


f :: [Int] -> Int
f n = length n


f1 :: [Int] Int -> [Int]
f1 n 1 = [(last n) rem 2]
f1 n 2 = [hd n .. last n]
f1 [a,b,c] 3 = [a * b^c ]
f1 [a,b,c,d] 4 = [(a+b),(c+d)]




//Start = Listing [5] //[1]

//Start = Listing [4,10] //[4,5,6,7,8,9,10]

//Start = Listing [3,5,2] //[75]

Start = Listing [13,29,1030,307] //[42,1337]

//Start = Listing [] //[]