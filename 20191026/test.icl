module test
import StdEnv


f1 :: [Int] -> Bool
f1 n = isMember False (map (\x = isEven x ) n)


Start = f1 [1,2,3,4]
