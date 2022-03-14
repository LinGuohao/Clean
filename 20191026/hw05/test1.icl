module test1
import StdEnv


/*
Write a function which takes an integer and returns
True(boolean value) if the given number is factorial
of some integer number




*/
isFactorial::Int->Bool
isFactorial n 
|n < 0 = False
= isMember n (List n)

Factorial :: Int -> Int
Factorial 1 = 1
Factorial n = n * Factorial (n-1)

List :: Int -> [Int]
List 1 = [1]
List n = [Factorial n]  ++ List (n-1)
//Start = isFactorial 34



//Start=isFactorial 34 //False
//Start=isFactorial 120 //True
//Start=isFactorial  1 //True
//Start=isFactorial -485//False