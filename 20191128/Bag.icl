implementation module Bag

import StdEnv


// Implement Multiset (aka Bag, aka mset) data structure
// It stores elements and their respective multiplicities.
// Your task is to implement methods from defintion module, which is given.


:: Bag a :== [(Int,a)]

newB :: (Bag a)
newB = []

isempty :: (Bag a) -> Bool
isempty [] = True
isempty _  = False

insertB :: a (Bag a) -> Bag a | Eq a
insertB z [] = [(1,z)]
insertB z [x:y]
|z == (snd x) = [(((fst x)+1),z)] ++ y 
= insertB z y

insertnB :: a Int (Bag a) -> Bag a | Eq a
insertnB z n [] = [(n,z)]
insertnB z n [x:xs]
|z == snd x = [(((fst x)+ n),z)] ++ xs
= insertnB z n xs



removeB    ::  a  (Bag a) -> Bag a  | Eq a 
removeB x [] = []
removeB x [y:ys]
|x == (snd y) = [(((fst y) - 1) , snd y) : ys]
= removeB x ys


removenB     :: a Int (Bag a) -> Bag a  | Eq a 
removenB x n [] = []
removenB x n [y:ys]
|x == (snd y) = [(((fst y) - n) , snd y) : ys]
= removenB x n ys

sizeB      ::     (Bag a) -> Int
sizeB [] = 0
sizeB [x:xs] = (fst x) + sizeB xs 




sizediffB ::     (Bag a) -> Int
sizediffB x = (length x)



items ::     (Bag a) -> [a] 
items [] = []
items [x:xs] = [snd x] ++ items xs

multiplicity  ::   a  (Bag a) -> Int | Eq a
multiplicity x [] = 0
multiplicity x [y:ys]
|x == snd y = fst y
= multiplicity x ys

// tests of implementations:
// uncomment everything

Start				= ( "s0 = newB = ",        s0,'\n'
					  , "s1 = insertB 1 s0 = ",       s1,'\n'
					  , "s2 = insertB 1 s1 = ",       s2,'\n'
					  , "s3 = insertB 2 s2 = ",       s3,'\n'
					  , "s4 = removeB 1 s3 = ",       s4,'\n'
					  , "s5 = sizeB s3 = ",          s5,'\n'
					  , "test = isempty s3 = ",     test,'\n'
					  )
where
	s0				= newB
	s1				= insertB   1      s0
	s2				= insertB   1      s1
	s3				= insertB   2      s2
	s4				= removeB   1      s3
	s5				= sizeB          s3
	test            = isempty         s3


