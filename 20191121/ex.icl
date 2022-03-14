module ex
import StdEnv

/*
:: Tree a = Noda a (Tree a)  (Tree a)
| Leaf

mapfun :: (Tree Int) -> (Tree Int)
mapfun Leaf = Leaf 
mapfun (Node x l r) = (Node (x+1) (mapfun l) (mapfun r))



preorder :: (Tree Int) -> [Int]
preorder Leaf = []
preorder (Node x l r ) = preorder l ++ preorder r ++ [x]

//Start = Preorder (Node 2 (Node 3 Leaf Leaf)(Node 5 Leaf Leaf))

*/

artol :: {a} -> [a]
artol ar  = [e \\ e <-: ar]

Start = artol {1,2,3}