module hw3

import StdEnv
/**
  * Write a function that takes an integer 'x'
  * and checks if this number is a palindrome.
  * A palindrome is a number or word that is identical 
  * when written forward or backwards.
  *
  * e.g. 1234 is not a palindrome. 145626541 is a palindrome.
  * Total: 40pts
  */
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

//Start = isPalindrome 0 //True
//Start = isPalindrome 55 //True
//Start = isPalindrome 49594 //True
//Start = isPalindrome 1337 //False
//Start = isPalindrome (~57975) //False


//if (n < 10) {
	//return [n]

//else {
	//return ( intToList(n/10) ++ [n rem 10] )
//}