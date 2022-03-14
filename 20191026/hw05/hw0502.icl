module hw0502
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
//Start=isFactorial 34 //False
//Start=isFactorial 120 //True
//Start=isFactorial  1 //True
//Start=isFactorial -485//False

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
//Start = median [3,98,2,7,1,76,3,775,4,22] //5.5

/*
Given a list of tuples (a,b) -a,b integer numbers- such that
a and b are the sides  of a rectangle.
Return a list of triples (a,b,c) where c is the area of a rectangle
with dimensions a and b if and only if the area of that
rectangle is between 20 and 30*/
areaBetween20and30 :: [(Int,Int)] -> [(Int,Int,Int)]
areaBetween20and30 n =  f4 (list n)

list :: [(Int,Int)] -> [(Int,Int,Int)]
list n = map(\x = f x) n

f:: (Int,Int) -> (Int,Int,Int)
f (a,b) = (a,b,a*b)

f1 :: (Int,Int,Int) -> Bool
f1 (a,b,c) = c > 20 && c < 30

f4 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
f4  n = filter f1  n 
//Start= areaBetween20and30 [(1,34),(4,3),(5,5),(9,6),(3,9)]//[(5,5,25),(3,9,27)]
//Start=areaBetween20and30 [(5,6),(4,5),(20,1),(30,1)]//[]
//Start= areaBetween20and30 [(1,23),(7,3),(5,5),(7,6),(3,8),(12,2),(24,0),(0,0)]//[(1,23,23),(7,3,21),(5,5,25),(3,8,24),(12,2,24)]

:: Vector2 a = {v0 :: a, v1 :: a}

instance ==   (Vector2 a) | == a   where == vector0 vector1	= vector0.v0 == vector1.v0 && vector0.v1 == vector1.v1

instance zero (Vector2 a) | zero a where zero	= {v0 = zero, v1 = zero}

instance one  (Vector2 a) | one a  where one 	= {v0 = one, v1 = one}

instance ~    (Vector2 a) | ~ a    where ~ vector0	= {v0 = ~vector0.v0, v1 = ~vector0.v1}

instance +    (Vector2 a) | + a    where + vector0 vector1	= {v0 = (vector0.v0 + vector1.v0), v1 = (vector0.v1 + vector1.v1)}

instance -    (Vector2 a) | - a    where - vector0 vector1	= {v0 = (vector0.v0 - vector1.v0), v1 = (vector0.v1 - vector1.v1)}

instance *    (Vector2 a) | * a    where * vector0 vector1	= {v0 = (vector0.v0 * vector1.v0), v1 = (vector0.v1 * vector1.v1)}

instance /    (Vector2 a) | / a    where / vector0 vector1	= {v0 = (vector0.v0 / vector1.v0), v1 = (vector0.v1 / vector1.v1)}

//Start	= {v0 = 5, v1 = 6} + one //(Vector2 6 7)


:: Vector3 a = {x0 :: a, x1 :: a, x2 :: a}

/*
Implement instances : zero, one, ~, +, -, *, /  for the Vector3 type( you can refer to Vector2)
 e.g. instance ==   (Vector3 a) | == a   where == vector0 vector1	= vector0.x0 == vector1.x0 && vector0.x1 == vector1.x1 && vector0.x2 == vector1.x2
*/
instance ==   (Vector3 a) | == a where == vector0 vector1 = vector0.x0 ==vector1.x0 && vector0.x1 == vector1.x1 && vector1.x2 == vector1.x2
instance zero (Vector3 a) | zero a where zero = {x0 = zero ,x1 = zero,x2 = zero}
instance one  (Vector3 a) | one a where one = {x0 = one , x1 = one ,x2 = one}
instance ~    (Vector3 a) | ~ a where ~ vector0 = {x0 = ~vector0.x0 , x1 = ~vector0.x1 , x2 = ~vector0.x2}
instance +    (Vector3 a) | + a where + vector0 vector1 = {x0 = (vector0.x0 + vector1.x0), x1 =(vector0.x1 + vector1.x1), x2 = (vector0.x2 + vector1.x2)}
instance -    (Vector3 a) | - a where - vector0 vector1 =  {x0 = (vector0.x0 - vector1.x0), x1 = (vector0.x1 - vector1.x1),x2 = (vector0.x2 - vector1.x2)}
instance *    (Vector3 a) | * a where * vector0 vector1	= {x0 = (vector0.x0 * vector1.x0), x1 = (vector0.x1 * vector1.x1), x2 = (vector0.x2 * vector1.x2)}

instance /    (Vector3 a) | / a where / vector0 vector1	= {x0 = (vector0.x0 / vector1.x0), x1 = (vector0.x1 / vector1.x1),x2 = (vector0.x2 / vector1.x2)}
Start	= {x0 = 5, x1 = 6 , x2 = 7} + one //(Vector3 6 7 8)
/*
Use mathematical knowledge related to vector dot product to implement the following function:
*/
Vec3dotProduct :: (Vector3 a) (Vector3 a) -> a  | *,+ a 
Vec3dotProduct vec1 vec2 = vec1.x0 * vec2.x0 + vec1.x1 * vec2.x1 + vec1.x2 * vec2.x2
//Start = Vec3dotProduct {x0 = 1.5, x1 = 2.6, x2 = 3.0} {x0 = 5.0, x1 = 2.6, x2 = 4.5} //27.76 


//Start = Vec3dotProduct {x0 = 1.5, x1 = 2.6, x2 = 3.0} {x0 = 5.0, x1 = 2.6, x2 = 4.5} //27.76 

/*
Use mathematical knowledge related to vector cross product to implement the following function:
*/
Vec3crossProduct ::  (Vector3 a) (Vector3 a) ->  (Vector3 a) | *,-a
Vec3crossProduct vec1 vec2 = {x0 = ((vec1.x1 * vec2.x2) - (vec1.x2 * vec2.x1)) , x1 = ((vec1.x2 * vec2.x0) - (vec1.x0 * vec2.x2)), x2 = ((vec1.x0 * vec2.x1) - (vec1.x1 * vec2.x0))}

//Start = Vec3crossProduct {x0 = 1.5, x1 = 2.6, x2 = 3.0} {x0 = 5.0, x1 = 2.6, x2 = 4.5} //(Vector3 3.9 8.25 -9.1)