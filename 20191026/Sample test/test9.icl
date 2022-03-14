module test9
import StdEnv



//foldl 0-1-2-3-4 = -10
//foldr 4-3-2-1-0 = -2       (-)0 [1..4] 



/**9
//Use foldr to check if the square root of each integer in a list are all integers. */
*/
f9::[Int] ->Bool
f9 [] = True
f9 n =  foldr (+)0 n  iterate 


|| && and or

//sqrt 9.0 = 3



Start = f9 [] //True

//Start = f9 [4,16,9] //True

//Start = f9 [1,8] //False