module SampleEndterm1
import StdEnv

// Solve as many functions as you can. Each exercise is of 10%, to pass min. 40% is necessary.
// marks: 40%-2,60%-3,80%-4,100%-5. 
:: Tree a = Node a (Tree a) (Tree a)| Leaf 


//1.
//Define a tree type and use the followings for testing your solution.

tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf



//Write a function that takes a tree as a parameter and returns a list of nodes which have at least one prime child.
//An empty tree will return [].
instance == (Tree Int)
       where 
       == Leaf Leaf = True
       == _ _  = False 

goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r


subTreeList :: (Tree a) -> [(Tree a)]
subTreeList Leaf = []
subTreeList tree = subTreeList(goL tree) ++ [tree] ++ subTreeList(goR tree)


extractSubLists :: a (Tree a) -> [(Tree a)] | Eq a
extractSubLists n tree = [subtree\\subtree<-(subTreeList tree)|(extractNode subtree)==n]

getChildren :: a (Tree a) -> [a] | Eq a
getChildren n tree
| isLeaf(goL subtree)&&isLeaf(goR subtree)=[]
| isLeaf(goL subtree) = [extractNode(goR subtree)]
| isLeaf(goR subtree) = [extractNode(goL subtree)]
= [extractNode(goL subtree)]++[extractNode(goR subtree)]
	where
		subtree = hd(extractSubLists n tree)


primeChildren :: (Tree Int) -> [Int]
primeChildren Leaf = []
primeChildren (Node x l r)
| (l == Leaf ) && (r == Leaf) = []
|[ z \\ z <- getChildren x (Node x l r)| isprime z ] <> [] = [x] ++ primeChildren l ++ primeChildren r
= primeChildren l ++ primeChildren r



isprime :: Int -> Bool 
isprime  z
|z == 0 = False
|z == 1 = False
=[x\\ x <- [2..(z/2)]| z rem x == 0] == [] 

//Start = primeChildren tree1 //[10,7]
Start = primeChildren tree2 //[0,4,8]
//Start = primeChildren unitTree //[]
//Start = primeChildren noTree //[]

//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.
symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (x,y)  
|[z\\z <-: x] == [ m \\m <-: y] = [ m \\m <-: y]
=  f2 ([z\\z <-: x] ++ [ m \\m <-: y])

f2 :: [Int] -> [Int]
f2 [] = []
f2 [x:xs] 
|isMember x xs == False = [x] ++ f2 xs
= f2 xs


//Start = symmetricDiff ({1,2,3,4},{3,4,5,6}) //[1,2,5,6]
//Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
//Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
//Start = symmetricDiff ({1,2,3,4},{}) //[1,2,3,4]
//Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]
//Start = symmetricDiff ({},{}) //[]


:: Q = {nom :: Int , den :: Int}


//3.
//Define a Q type for rational numbers.
//Write a function that receives two fractions and calculates their division. Simplify the fraction before returning.
//In case the nominator is zero, set the ·ÖÄ¸denominator to zero as well.
fracDivision :: Q Q -> Q
fracDivision x y 
|x.nom * y.den == 0 = {nom = 0, den = 0}
= f3 {nom = x.nom * y.den , den = x.den * y.nom} 

f3 :: Q -> Q
f3 {nom = n , den = d} = {nom = n /g ,den = d/g}
where g = gcdm n d
  gcdm x y = gcdnat (abs x) (abs y)
  where gcdnat x 0 = x
        gcdnat x y = gcdnat y (x rem y)


//Start = fracDivision {nom=5, den=1} {nom=6, den=5} //{nom=25, den=6}
//Start = fracDivision {nom=10, den=2} {nom=3, den=4} //{nom=20, den=3}
//Start = fracDivision {nom=0, den=2} {nom=100, den=4} //{nom=0, den=0}
//Start = fracDivision {nom=15, den=2} {nom=3, den=12} //{nom=30, den=1}

half = { nom=1, den=2 }
third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)			
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)

//4.
//Write a function that will return the sum of a tree's nodes.
//Return the sum as a simplified Q.

treetolist :: (Tree Q) -> [Int]
treetolist Leaf = []
treetolist (Node x l r) = [x.den] ++ treetolist l ++ treetolist r

get :: [Int] -> Int
get x = lcm (hd x) (last x)


sumTree :: (Tree Q) -> Q
sumTree Leaf = {nom = 0, den = 1}
sumTree x
= f3{nom = plusleaf x , den = 30}


plusleaf :: (Tree Q) -> Int
plusleaf Leaf = 0
plusleaf (Node x l r)
|x.den <> get (treetolist (Node x l r)) = (x.nom * (30/ x.den)) + plusleaf l + plusleaf r  
 = x.nom + plusleaf l + plusleaf r


 
//Start = sumTree smallTree //{nom=3,den=1}
//Start = sumTree bigTree //{nom=97,den=15}
//Start = sumTree miniTree //{nom=19, den=20}

//5.
//Write a function that will check if a tree of Q is a Binary Search Tree.
checkTree :: (Tree a) -> Bool | Ord a
checkTree (Node x l r)
| isLeaf l && isLeaf r = True
| isLeaf l && x<(extractNode r) = True &&(checkTree r)
| isLeaf r && (extractNode l)<x = True &&(checkTree l)
| (extractNode l)<x&&x<(extractNode r) = True && (checkTree l)&&(checkTree r)
= False

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

extractNode :: (Tree a) -> a
extractNode (Node x l r) = x


instance < Q
   where 
   <  q1 q2 = (f5 q1 q2)
   
   
f5 :: Q Q -> Bool
f5 q1 q2
| (q1.nom *(lcm q1.den q2.den)) > (q2.nom *(lcm q1.den q2.den)) = True
= False 
   
   


//Start = checkTree bigTree //True
//Start = checkTree smallTree //True
//Start = checkTree badTree //False




//:: Color = Red | Yellow | Green | Blue | Purple | Violet
//:: ColorCombo = { color1 :: Color, color2 :: Color}

//6.
//Write a function that when given a color, returns its complement.
:: Color = Red | Blue | Green | Purple | Violet | Yellow

//That is:
//Red -> Blue, Yellow -> Purple, Green -> Violet, Blue -> Red, Purple -> Yellow, Violet -> Green
colorComp :: Color -> Color
colorComp  Red = Blue
colorComp Blue = Red
colorComp Green = Violet
colorComp Purple = Yellow 
//Start = colorComp Red //Blue
//Start = colorComp Blue //Red
//Start = colorComp Green //Violet
//Start = colorComp Purple //Yellow


:: ColorCombo = {color1 :: Color, color2 :: Color}
//7.
//Write a function that when given a Color, creates a list of possible color combos.
//Valid color combos can not have duplicate colors.
colorlist = [Red,Blue,Green,Purple,Violet,Yellow]
colorCombo :: Color -> [ColorCombo]
colorCombo x = [{color1 = x,color2 = z} \\ z <- colorlist | z <> x]


instance == Color
   where 
   == Red Red = True
   == Blue Blue = True
   == Green Green = True
   == Purple Purple = True
   == Violet Violet = True
   == Yellow Yellow = True
   == _ _ = False
         
//Start = colorCombo Red //[{color1=Red, color2=Yellow},{color1=Red, color2=Green},{color1=Red, color2=Blue},{color1=Red, color2=Purple},{color1=Red, color2=Violet}]
//Start = colorCombo Blue //[{color1=Blue, color2=Red},{color1=Blue, color2=Yellow},{color1=Blue, color2=Green},{color1=Blue, color2=Purple},{color1=Blue, color2=Violet}]

//8.
//Amicable numbers are two different numbers so related that the sum of the proper divisors of each 
//is equal to the other number. (A proper divisor of a number is a positive factor of that number other than the number itself. 
//For example, the proper divisors of 6 are 1, 2, and 3.) 
//Check if two integers are amicable pairs and put them together with the answer in a bag.
//amicable pair: 1184 and 1210 
//proper divisor of 1184 :  1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592 -> their sum == 1210
//proper divisor of 1210 : 1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605 -> their sum == 1184
:: Bag a :==[((Int,Int),Bool)]

amiBag :: [(Int,Int)] -> Bag a
amiBag [] = []
amiBag x  = [(z,f8 z)\\ z <- x]


f8 :: (Int,Int) -> Bool
f8 (a,b) 
|(sum(getdiv a) == b && sum (getdiv b) == a) = True
=False



getdiv :: Int -> [Int]
getdiv  x =   [ z\\z<-[1..(x/2)] | x rem z == 0 ]



//Start = amiBag [(1184,1210), (13,245)]
//Start = amiBag [] // []
//all true
//Start = amiBag [(220, 284), (1184, 1210), (2620, 2924), (5020, 5564), (6232, 6368), (10744, 10856), (12285, 14595), (17296, 18416), (63020, 76084), (66928, 66992)]

//9.
//Given the Object type, compute for the state component the given method and print the result as a String.
//ex: for state 3 compute 3 + 1 using the given method, and print the result "4" as string.
:: Object = {state::Int, method::Int->Int, tostring:: Int -> String }
MyObject = { state = 3 ,method = (+) 1, tostring = toString}






f9 :: Object -> String 
f9  x = x.tostring (x.method x.state)

//Start = f9 MyObject
//10.
//Given an operator and two lists, apply the operator to the elements of 
//the same positions of lists, like in the examples.

:: Operator a :== a a -> a

              
op2 :: (Operator a) [a] [a] -> [a] | + , * a
op2 x y z = [ x m  q\\m <- y & q <- z]


//Start = 3 4

//Start = op2 (*) [2,3,4,5] [7,8,9,10] // [14,24,36,50]
//Start = op2 (*) [2,3,4,5] [7,8] // [14,24]
//Start = op2 (*) [2,3] [7,8,9,10] // [14,24]
//Start :: [Int]
//Start = op2 (*) [] [] // []
//Start = op2 (+) [3,2] [7,8] // [10,10]