module cw01g1
import StdEnv


// Given two integers, write a function
// that will give us their least common multiple.
f1 :: Int Int -> Int
f1 x y
|x == 0 = 0
|y == 0 = 0
= lcm(abs x)(abs y)




 
//Start = f1 3 4 //12
//Start = f1 0 5 //0
//Start = f1 ~7 4 //28
//Start = f1 12 10 //60

// Given 'n' number of friends
// and 'm' pieces of cake, 
// how many different ways are there to
// distribute these pieces of cake?
// f2 :: Int Int -> Int
// Start = f2 5 2 //10
// Start = f2 5 8 //56
// Start = f2 5 ~13 //0
// Start = f2 ~4 9999 //0

// Given an integer, write a function
// that will check if each digit is even.

f3 :: Int -> Bool
f3 n   
|isMember False (map (\x = isEven x) (list n)) == True = False
= True

list :: Int -> [Int]
list n 
|n == 0 = [] 
=[n rem 10] ++ list (n/10)

//Start = f3 1234 //False
//Start = f3 506 //True
//Start = list (~846) //True