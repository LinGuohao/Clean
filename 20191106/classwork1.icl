module classwork1
import StdEnv

// Given an integer, write a function
// that returns the sum of all
// odd numbers less than it.
// f1 :: Int -> Int
// Start = f1 5 //4
// Start = f1 12 //36
// Start = f1 ~6 //0
f1 :: Int -> Int
f1 n =  flaux(n-1)
flaux :: int -> int
|n <= 0 = 0  
| isEven n = flaux (n-1)
= n+ f1aux(n-2)

Start =  f1 ~6 


// How many different ways are there
// to distribute 'n' homework problems
// amongst 'm' number of friends?
// f2 :: Int Int -> Int
// Start = f2 200 4 //64684950
// Start = f2 2 4 //6
// Start = f2 ~4 4 //0
// Start = f2 9999999 0 //0
fact 1 = 1 
fact x = x* fact(x-1)
f2 :: Int Int -> Int 
f2 n m
|n <=0 || m<=0 = 0
|n>= m = (fact n)/ (fact(n-m)*(fact m))
|n< m = f2 m n

Start f2 200 4



// Given an integer, write a function
// that will sum up its digits.
// f3 :: Int -> Int
// Start = f3 1234 //10
// Start = f3 506 //11
// Start = f3 6 //6
// Start = f3 ~91 //10
f3 :: Int -> Int 
f3 x = f3 x 0
f3aux :: Int Int -> Int 
f3 x acc
|x <= 0 =0
=(x rem 10) + f3aux(x/10)
