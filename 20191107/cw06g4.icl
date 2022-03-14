module cw06g4
import StdEnv


f3 :: Int -> Int
f3 n  = length([(x,y,z)\\ (x,y) <- (f1 n) & z <- (f2 n)])


f1 :: Int -> [(Int,Int)]
f1 n = [(a,b)\\ a  <- [1..n] , b <- [1..n] ]

f2 :: [(Int,Int)] -> [Int]
f2 n = filter ((<)(n^2))( map (\x = (fst x)^2 + (snd x)^2) (f1 n))

Start = f3 100
      