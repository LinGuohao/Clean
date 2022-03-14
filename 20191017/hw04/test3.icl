module test3
import StdEnv
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