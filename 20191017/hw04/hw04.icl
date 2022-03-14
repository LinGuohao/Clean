module hw04
import StdEnv

/**
  * 30 pts
  * Write a function that takes a list of integers
  * and returns the variance of the list.
  * That is, the sum of the square differences from the mean divided by
  * number of elements - 1.
  * For example, variance of [1,2,3,4,5] is calculated by:
  * Mean = (1+2+3+4+5)/5 = 3
  * Sum of Square Differences = (1-3)^2 + (2-3)^2 + (3-3)^2 + (4-3)^2 + (5-3)^2 = 10
  * Variance = 10/(5-1) = 10/4 = 2.5
  *
  * Note: Your solution must use 'map' or a list comprehension.
  */
//Variance :: [Int] -> Real
//Start = Variance [1..5] //2.5
//Start = Variance [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] //0
//Start = Variance [-4,1,6,0,-2,6] //16.96666666666
//Start = Variance [] //0
Variance :: [Int]-> Real
Variance [] = 0.0
Variance n = SOSD n / toReal((length n)-1)

SOSD :: [Int] -> Real
SOSD [] = 0.0
SOSD n =  sum (map(\x = (x - (mean n))* (x - (mean n)))(map (toReal) n))

mean :: [Int]-> Real
mean [] = 0.0
mean n =  toReal((sum n))/toReal((length n))

Start = Variance [1..5]

/**
  * 30 pts
  * Write a function that takes a list of integers and reverses their digits and order.
  * 
  * For example: ReverseDig [123,456,789] = [987,654,321]
  */
//ReverseDig :: [Int] -> [Int]
//Start = ReverseDig [123,456,789] //[987,654,321]
//Start = ReverseDig [] //[]
//Start = ReverseDig [1,23,456,7891,23456] //[65432,1987,654,32,1]
ReverseDig :: [Int] -> [Int]
ReverseDig [] = []
ReverseDig n = reverse (Reverse n)



Reverse::[Int]->[Int]
Reverse [] = []
Reverse [a:b] =  [(reint (list a))] ++ (Reverse b)

list :: Int -> [Int]
list a
| a == 0 = []
= [a rem 10] ++ list (a/10)

reint:: [Int] -> Int
reint [] = 0
reint [c] = c
reint [c:d] = c * 10 ^((length [c:d]) - 1) + reint d 




Start =  ReverseDig [1,23,456,7891,23456]

/**
  * 40 pts
  * Write a function that takes a predicate (a -> Bool function) and
  * a list of sublists of integers and returns the sum of all elements that
  * return True on both  or one of the two predicates depending on
  * the given parameter "or"/"and".
  */
//FilterSum :: (Int -> Bool) String (Int -> Bool) [[Int]] -> Int
//Start = FilterSum isEven "or" ((<) 3) [[1..5],[-2..10]] //60
//Start = FilterSum ((<)10) "and" (\x=isEmpty[div\\div<-[2..(x-1)]|x rem  div == 0]) [[1..20],[90..100],[1..10]] //157
//Start = FilterSum isOdd "or" isEven []//0

FilterSum :: (Int -> Bool) String (Int -> Bool) [[Int]] -> Int
FilterSum  b a c [] = 0
FilterSum  b a c [e:f]
|a == "or" = sum(filter b e )  + sum(filter c (filter (not o b) e) )+ FilterSum b a c f
|a == "and" = sum(filter c (filter b e)) + FilterSum b a c f
 





Start = FilterSum isOdd "or" isEven []//0