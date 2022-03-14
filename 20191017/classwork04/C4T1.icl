module C4T1 
import StdEnv

fun x =  not(ispower (abs x))

f2 :: [Int] -> [Int]
f2 a = filter fun a

ispower :: Int -> Bool
ispower 1 = True
ispower 2 = True
ispower 0 = False
ispower x
| isOdd x = False
= ispower (x/2)

Start = f2 [1, -2, 3, 5, 7, -9, 11]