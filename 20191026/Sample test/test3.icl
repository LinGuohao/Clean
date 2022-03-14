module test3
import StdEnv




/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

//SeqCheck :: [Int] -> Bool

SeqCheck :: [Int] -> Bool
SeqCheck n = isEmpty(filter (not o isOdd) (list1 n))  && isEmpty (filter (not o isEven) (list2 n))  

list1 :: [Int] -> [Int]
list1 [] = []
list1 n = take 1 n ++ list1 (drop 2 n) 

list2 :: [Int] -> [Int]
list2 [] = [] 
list2 n = [ n !! 1] ++ list2 (drop 2 n)
//Start = SeqCheck [1..10] //True

Start = SeqCheck [1,2,3] //True

//Start = SeqCheck [2,3,4] //False

//Start = SeqCheck [1,3,4,5] //False

//Start = SeqCheck [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False
