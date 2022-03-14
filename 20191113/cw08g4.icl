module cw08g4
import StdEnv

/*
Given an array of Int and a single Int, use array
comprehension to double each element of the array,
keeping only the multiples of the second Int argument.
*/
f1 :: {Int} Int -> {Int}
f1 a b = {x*2\\ x <-:a |(x*2) rem b == 0 }



//Start = f1 {1,2,3,4} 4 //{4,8}
//Start = f1 {3,4,5,7,2,9} 3 //{6,18}

/*
Given a Tree with nodes of type Person,
return the number of people who are older than 18.
That is, people born on or before 2001.11.22
*/
::Person = { name::String
			,birthday::(Int,Int,Int)
	}
::Tree a = Node a (Tree a) (Tree a)
	|Leaf

t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2002,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)

isodder :: (Int,Int,Int) -> Bool
isodder n 
|fst3 n < 2001 = True
|fst3 n == 2001 && snd3 n < 11 = True
|fst3 n == 2001 && snd3 n == 11 && thd3 n <= 22 = True
=False


f2 :: (Tree Person) -> Int
f2 Leaf = 0
f2 (Node x l r)   
|isodder x.birthday   = 1 + f2 l + f2 r
= f2 l + f2 r
//Start = f2 t2 //2
//Start = f2 t3  //3

/*
Given a Tree of type Person, return the same tree, except
with "_qualify" attached to the end of the names of each person
who is over 18.
*/
f3 :: (Tree Person) -> (Tree Person)
f3 Leaf = Leaf
f3 (Node x l r) 
|isodder x.birthday = Node {x &name = (x.name +++ "_qualify"),birthday = (3,4,5)} (f3 l) (f3 r)
=Node x (f3 l) (f3 r)

Start = f3 t2 //(Node (Person "hh_qualify" (2001,11,22)) (Node (Person "hr_qualify" (2001,11,21)) Leaf Leaf) (Node (Person "ht" (2001,11,23)) Leaf Leaf))
//Start = f3 t3  //(Node (Person "hh_qualify" (2001,11,22)) (Node (Person "hr_qualify" (2001,11,21)) (Node (Person "hh" (2002,11,22)) Leaf Leaf) (Node (Person "hh_qualify" (1998,11,22)) Leaf Leaf)) (Node (Person "ht" (2001,11,23)) Leaf Leaf))