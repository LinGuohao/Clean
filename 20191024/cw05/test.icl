module test
import StdEnv
faux :: Int -> [Int]
faux x = take x [x-1\\ x <- [x,x-1..2]] 
LeapYears :: [Int] -> [Int]
LeapYears [h:t] = [h\\ h <- [h:t] | (h rem 4) == 0 && (h rem 100) <> 0 || (h rem 400) == 0]
LeapYears1 :: Int -> [Int]
LeapYears1 x = reverse(LeapYears (faux x)) 
Start = LeapYears1  400