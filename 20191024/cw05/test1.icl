module test1
import StdEnv

// 2. Generate a list of leap year with the maximum element smaller than a given Integer 
// How to find leap year?
 // if (year is not divisible by 4) then (it is a common year) 
 // else if (year is not divisible by 100) then (it is a leap year)
  // else if (year is not divisible by 400) then (it is a common year) 
  // else (it is a leap year)

list :: Int ->[Int]
list x = takeWhile ((>)x) (iterate ((+)4)  4)
LeapYears :: [Int] -> [Int]
LeapYears [h:t] = [h\\ h <- [h:t] |(h rem 100) <> 0 || (h rem 400) == 0]
LeapYears1 :: Int -> [Int]
LeapYears1 x = (LeapYears (list x)) 
Start = LeapYears1  404