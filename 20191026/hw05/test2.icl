module test2
import StdEnv

/*
Write a function which takes a list of integers and determines
the median of this list.
Definition for median : https://en.wikipedia.org/wiki/Median
Example : median [1,2,3,4,5,6,7,8], 
the median is (4+5)/2 = 4.5
*/
median :: [Int] -> Real
median [] = 0.0
median n =  f3 (f1 (sort n) (f n)) (f n) //f3 (f1 (n f n)) (f n)


f3:: ([Int],[Int]) Int -> Real
f3 (a,b) 0 = toReal((last a + hd b))/2.0
f3 (a,b) 1 = toReal (hd b)


f1:: [Int] Int -> ([Int],[Int])
f1 n 0 = splitAt ((length n)/2) n
f1 n 1 = splitAt (((length n)- 1)/2) n

f :: [Int] -> Int
f n 
|(length n) rem 2 == 0 = 0
= 1 





//Start = median [1..9] //5
Start = median [3,98,2,7,1,76,3,775,4,22] //5.5