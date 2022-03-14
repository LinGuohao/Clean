module tree
import StdEnv




::Tree a = Node a (Three a) (Three a) | Poop

boradThree :: (Three Int)
boradThree = Node 5 (Node 2 (Node 1 Poop Poop)(Node 3 Poop Poop))(Node 14 (Node 9 (Node 7 (Poop)(Poop))(Node 11 Poop Poop)) (Node 20 (Node 17 ()() Poop)