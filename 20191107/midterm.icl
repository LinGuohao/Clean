module midterm
import StdEnv

/*
1.
Given a list of sublists of Int
Write a function which returns a list containing
the minimum of every sublist
You should use foldr when finding the minimum.
Ignore empty sublists.
*/

//f :: [[Int]] -> [Int]
//f [] = []
//f n = map [\x =  foldr (+) 0 [hd a]]  n 

//Start = GetMin [[42, 420], [24, 240]] // [42, 24]
//Start = GetMin [[], [1], [2,3], [4,5,6]] // [1, 2, 4]
//Start = GetMin [[], [1], [2, ~3], [4, ~5, 6]] // [1, -3, -5]
//Start = GetMin [[]] // []


/*
2.
Given a list of tuples, return a single tuple which is
the sum of all tuple (x,y) in which x,y have the same parity (odd, odd)/(even, even)
minus the sum of all tuple (x,y) in which x,y dont have the same parity (odd, even)/(even, odd)
For example:
TupleSum [(1, 2), (4, 4)]
(4,4) has the same parity. We take that and subtract (1,2), which has opposite parity.
*/

TupleSum :: [(Int,Int)] -> (Int,Int)
TupleSum [] = (0,0)
TupleSum n = ((Add n) ,(Add2 n)) 

Add2 ::[(Int,Int)] -> Int
Add2 n =  sum[ (snd x)\\x <- (relist n)]


Add :: [(Int,Int)] -> Int
Add n =  sum[ (fst x)\\x <- (relist n)] 
 
relist:: [(Int, Int)] -> [(Int, Int)]
relist [] = [(0,0)]
relist n = map (\x =  (f x)) n


f :: (Int,Int) -> (Int,Int)
f (a,b) 
|isEven(a+b) = (a,b)  
= ((0 - a),(0 - b))



//Start = TupleSum [] // (0,0)
//Start = TupleSum [(1,2)]
//Start = TupleSum [(1, 2), (4, 4)] // (3, 2)
//Start = TupleSum [(2, 6),(3, 4),(1, 2),(5, 9),(3, 6)] //(0,3)


/*
3.
Write a function that takes a list of tuples,
each tuple consisting of a predicate function and
a list of Int.
Return a list containing the sum of the sublists
where all elements return True for the predicate.
For example:
Start = conditionalFun [(isEven,[1..10]),(isOdd,[1,3,5,7]),(((<)3),[5..10])]
[1..10] does not return True to isEven for all elements.
[1,3,5,7] returns True for isOdd for all elements. We sum it to 16, add to list.
[5..10] returns True for greater than 3 for all elements. We sum to 45, add to list.
*/
conditionalFun :: [((Int->Bool),[Int])] -> [Int]
conditionalFun [] = []


//Start = conditionalFun [(isEven,[1..10]),(isOdd,[1,3,5,7]),(((<)3),[5..10])] //[16,45]
//Start = conditionalFun [((\x = True),[4,7..35]),((\x = False),[1..])] //[209]
//Start = conditionalFun [(isEven,[]),(isOdd,[])] //[0,0]
//Start = conditionalFun [] //[]


/*
4.
Write a function which takes a list of numbers and returns a list
containing only the palindromes.
A palindrome is a number or word which is identical when
read backwards and forwards.
For example: 123 is NOT a palindrome. 12321 is a palindrome.
*/
Palindromes::[Int]->[Int]
Palindromes [] = []

Palindromes  n = [x\\ x <- n | isit x]

isit :: Int -> Bool
isit n 
|last (list n) <> hd (list n) = False
=True
list :: Int -> [Int]
list n
|n<10 = [n]
= [n rem 10] ++ list(n/10) 


//Start = Palindromes [1,212,43,55,727,123,100] // [1,212,55,727]
//Start = Palindromes [76,89,1223,998]//[]
//Start = Palindromes []//[]
//Start = Palindromes [33]//[33]


/*
5.
Write a function which generates a list of the first n leap years starting from
a year x. If either of the arguments is negative output an empty list.
A leap year is divisible by 4 but is NOT divisible by 100 UNLESS it is divisible by 400
From Wikipedia:
if (year is not divisible by 4) then (it is a common year)
else if (year is not divisible by 100) then (it is a leap year)
else if (year is not divisible by 400) then (it is a common year)
else (it is a leap year)
*/


LeapYears :: Int Int->[Int]
LeapYears n q
|n<0 = []
|q < 0 = []
LeapYears x y = take y [z\\z <- [x..] | z rem 4 == 0 || z rem 100 == 0 || z rem 400 == 0   ]


//Start=LeapYears 1999 4 // [2000,2004,2008,2012]
//Start = LeapYears 1804 7 //[1808,1812,1816,1820,1824,1828,1832]
//Start = LeapYears -2000 4 //[]
//Start = LeapYears 2000 -9//[]


/*
6.
Write a function that takes a number and determine whether it is a perfect number or not!
A perfect number is a natural number that equals the sum of all its proper divisors.
A proper divisor is every divisor of a number excluding the number itself.
Example : isPerfect 6 // True
(Because the proper divisors of 6 are 1, 2 and 3 so their sum is equal
to 6 so it is true)
*/
isPerfect :: Int -> Bool
isPerfect 0 = False
isPerfect n
|sum (list2 n) == n = True
= False



list2 :: Int -> [Int]
list2 n = [x\\ x <- [1..n] | n rem x  == 0 && x<>n ]


//Start = isPerfect 6 //True
//Start = isPerfect 496 //True
//Start = isPerfect 11//False
//Start = isPerfect 1 //False
//Start = isPerfect 0 //False
//Start = isPerfect -1 // False


/*
7.
Given two integers, return a list of all common divisors
of two intergers (excluding 1)
*/
divisors::Int Int -> [Int]
divisors x y = ([z\\ z <-[2..x] | x rem z  == 0  && y rem z == 0])



//Start = divisors 6 12 //[2,3,6]
//Start = divisors 7 12 //[]
//Start = divisors 9 15 //[3]
//Start = divisors 128 64 //[2,4,8,16,32,64]


/*
8.
Given a list of intergers
find all the cube numbers(n^3) and write (n) to the first list; for example 8 -> 2
(A cube number is a number that is the product of three numbers which are the same)
find all the numbers which are powers of 2 (2^n) and write the exponent n to the
second list; for example 64 -> 6
*/
//cubes2::[Int]->([Int],[Int])
//Start = cubes2 [64, 16, 24, 15, 1 , 8] //([4,1,2],[6,4,0,3])
//Start = cubes2 [1..10] //([1,2],[0,1,2,3])
//Start = cubes2 [25..60] //([3],[5])


/*
9.
Write a function that will take a list of Integers and rem
will return a list of tuples (a,b) where b is every prime index of the given 
list and a is the value of the list at that index.
Ignore 1 as a prime.
*/
isPrime :: Int -> Bool
isPrime x = f10 x (x-1)
f10:: Int Int -> Bool
f10 x n
|n==1 =True
|x rem n == 0 =False
=f10 x (n-1) 

OnlyPrimePosition::[Int]->[(Int,Int)]
OnlyPrimePosition [] = [] 
OnlyPrimePosition n =  [ ((n!!(x-1)),x)\\x <- [2..length(n)] | isPrime x  ]






//Start=OnlyPrimePosition []//[]
//Start=OnlyPrimePosition [1,5,8]//[(5,2),(8,3)]
//Start=OnlyPrimePosition [1..19]//[(2,2),(3,3),(5,5),(7,7),(11,11),(13,13),(17,17),(19,19)]
//Start=OnlyPrimePosition [1,-5,4,3,6,-5,-7,9,-10]//[(-5,2),(4,3),(6,5),(-7,7)]


/*
10.
Write function to calculate n-th Tribonacci number. 
The nth Tribonacci number is defined by the equation:
T(n) = T(n-1) + T(n-2) + T(n-3)
With the starting parameters:
T(0) = 0, T(1) = 0, T(2) = 1
Your solution must be implemented efficiently via
tail recursion.
*/
//T :: Int -> Int
//Start = T 1 // 0
//Start = T 10 // 44
//Start = T 20 // 19513
//Start = T 50 // 1697490356184
//Start = T 100 // 4130554068881925393


/*
11.
An m-digit Armstrong Number is a number which is equal to sum of digit’s m-th powers.
For example - 153 is a 3 digit Armstrong number: 153 = (1*1*1) + (5*5*5) + (3*3*3).
Write a function which finds the first n Armstrong Numbers
*/
//armstrong :: Int -> [Int]
//Start = armstrong 9 // [1,2,3,4,5,6,7,8,9]
//Start = armstrong 15 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208]
//Start = armstrong 20 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834]
//Start = armstrong 21 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725]


/*
Test Vectors, for your convenience.
a = {x0 = 1, x1 = 2, x2 = 1}
b = {x0 = 3, x1 = 2, x2 = 3}
c = {x0 = 1.0, x1 = 2.0, x2 = 3.0}
d = {x0 = 2.5, x1 = 5.0, x2 = 7.5}
e = {x0 = 4.0, x1 = 5.0, x2 = 6.0}
f = {x0 = 5, x1 = 10, x2 = 5}
*/

/*
12.
Define the record type Vector3 taking type 'a'
and define its instances for +,-,Eq,Ord,Zero.
Ord should be defined as one Vector3 is smaller than
another Vector3 when their distance from the origin
is smaller.
Distance from origin of a vector (x0,x1,x2) can be
calculated by the square root of (x0^2 + x1^2 + x2^2)
Test Vectors and Operations.
a = <1,2,1>
b = <3,2,3>
Zero = <0,0,0>
a + b = <4,4,4>
a - b = <-2,0,-2>
a == b = False
a == a = True
a < b = True
a > b = False
*/

/*
13.
Using your defined Vector3 record, determine if two Vector3
are linearly dependent.
Two vectors are linearly dependent if multiplying every component
of one vector with a factor will give you the other vector.
For example:
<1, 2, 3> and <2.5, 5, 7.5> are linearly dependent by a factor of
2.5
Test Vectors and Results
<1.0,2.0,3.0> <4.0,5.0,6.0> = False
<1.0,2.0,3.0> <2.5,5.0,7.5> = True
*/
//linearDependent :: (Vector3 Real) (Vector3 Real) -> Bool

/*
14.
One of the most important operations in Ray Tracing is calculating the determinant.
Calculate the determinant for two 3D vectors.
Additional Information:
Your three dimensional vector should be defined above as Vector3.
The determinant of two 3D vectors 'a' and 'b' can be calculated by:
x0 = (a.x1 * b.x2 - a.x2 * b.x1)
x1 = -1 * (a.x0 * b.x2 - a.x2 * b.x0)
x2 = (a.x0 * b.x1 - a.x1 * b.x0)
Test Vectors and Results
<1,2,1> x <3,2,3> = <4,0,-4>
<3,2,3> x <1,2,1>  = <-4,0,4>
<1,2,1> x <5,10,5> = <0,0,0>
*/
//determinant3DVector :: (Vector3 Int) (Vector3 Int) -> (Vector3 Int)

/*
15.
For Ray Tracing, we can record the result as (Red, Green, Blue) or RGB values for every pixel in a file. The file extension is called ppm.
TASK: Create a 6x8 matrix of RGB tuples which holds the values for the flag of Hungary.

Requirement: Use list comprehension.
Hint: Partition the problem into three smaller 2x8 matrices, one for each color, and concatenate them in the end.

Additional Information:
a. RGB values vary from 0 to 255. Use simple color combination to create colors
Ex: RED = (255,0,0)
Ex: GREEN = (0,255,0)
Ex: WHITE = (255,255,255)
b. The Hungarian flag is divided into three equal horizontal rectangles, colored red, white and green, from top to bottom.

Expected result:
[[(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0)],
 [(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0)],
 [(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255)],
 [(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255)],
 [(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0)],
 [(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0)]]
*/
//printHungarianFlag :: [[(Int, Int, Int)]]