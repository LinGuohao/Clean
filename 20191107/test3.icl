module test3
import StdEnv

/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

f3 :: Int -> [Int]
f3 1 = [1] 
f3 n =  f3 (n - 1) ++ [f4 n]

f4 :: Int -> Int
f4 1 = 1
f4 2 = 1
f4 n = f4 (n-1) + f4(n-2)



Start = f3 8

//True

//Start = SeqCheck [2,3,4] //False

//Start = SeqCheck [1,3,4,5] //False

//Start = SeqCheck [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False 
