module quiz
import StdEnv


Tree x = Node x (Tree x) (Tree x) (Tree x) | Poop
sumTree :: Tree x -> x | + x
sumTree Poop = 0
sumTree tree = getNode tree + sumTree (goL tree) + sumTree (goM tree) + sumTree (goR tree)
where
    getNode (Node x _ _ _) = x
    goL (Node _ l _ _ ) = l
    goM (Node _ _ m _ ) = m
    goR (Node _ _ _ r ) = r