module test4
import StdEnv



/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */
f :: [Int] -> Bool
f [] = False
f n =  isEven (length (removeDup n)) && isEven (length n)


//Start = f [1,1,2,2,2,2,3,5,3,5] // True

//Start = f [1,1,2,2,1] // False

Start = f [1,1,3,3,3,3,7,7,7,7,6,6,8,8,9,9,9]


//Start = f [] //False