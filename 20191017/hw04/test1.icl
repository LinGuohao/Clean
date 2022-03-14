module test1
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


