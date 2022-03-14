module quiz
import StdEnv


:: BTree a = Bin (BTree a) (BTree a)
	| Tip a

g :: (a -> b) (BTree a) -> BTree b
g f (Tip x) = Tip (f x) 
g f (Bin t1 t2) = Bin (g f t1) (g f t2) 

aBTree = Bin (Bin (Bin (Tip 1) (Tip 2))(Bin (Tip 3) (Tip 4))) (Tip 5)
Start = g inc aBTree