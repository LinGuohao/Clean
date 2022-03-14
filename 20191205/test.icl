module test
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


instance == (Tree a)
where    == Leaf Leaf = True
         == _ _ = False

primeChildren :: (Tree Int) -> [Int]
primeChildren Leaf = []
primeChildren (Node x l r)
|r == Leaf && l == Leaf = []
|judge l r == True = [x] ++ primeChildren l ++ primeChildren r
= primeChildren l ++ primeChildren r

judge :: (Tree Int) (Tree Int) -> Bool
judge x y
|x == Leaf = isprime (get y)
|y == Leaf = isprime (get x)
|isprime (get x) || isprime (get y) = True
= False

get :: (Tree Int) -> Int
get Leaf = 1
get (Node x l r) = x


isprime :: Int -> Bool 
isprime  z
|z == 0 = False
|z == 1 = False
=[x\\ x <- [2..(z/2)]| z rem x == 0] == [] 

//Start = primeChildren tree1 //[10,7]
//Start = primeChildren tree2 //[0,4,8]
//Start = primeChildren unitTree //[]
//Start = primeChildren noTree //[]

//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.
symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (x,y) = [z \\z <-: x | Not(isMember z (list y))] ++ [m\\ m <-: y | Not(isMember m (list x))] 

where Not True = False
      Not False = True

list :: {Int} -> [Int]
list x = [z\\z<-: x]



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
fracDivision x y = simply {nom = x.nom * y.den  ,den = x.den * y.nom}

simply :: Q -> Q
simply x 
|x.nom == 0 = {nom = 0 , den = 0}
={nom = x.nom/(gcd x.nom x.den),den = x.den /(gcd x.nom x.den)}



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

instance  + Q
where     
          + x y = {nom =(x.nom * y.den) + (x.den * y.nom) ,den = (x.den*y.den)}


                   
           


treetolist :: (Tree Q) -> [Q]
treetolist Leaf = []
treetolist (Node x l r) = [x] ++ treetolist l ++ treetolist r

add :: [Q] -> Q
add [] = {nom = 0 ,den =1}
add [x:xs] = x + add xs

sumTree :: (Tree Q) -> Q
sumTree x =  simply(add(treetolist x))



//Start =  sumTree smallTree //{nom=3,den=1}
//Start = sumTree bigTree //{nom=97,den=15}
//Start = sumTree miniTree //{nom=19, den=20}

Start = length([1 ,2 ,3], [1,2])

