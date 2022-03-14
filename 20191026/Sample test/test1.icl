module test1
import StdEnv

Router :: [(a->b)] [(Int,a)] -> [b]
Router [] b = []
Router a [] = []
Router a b = map (\(x,y) = (a !! (x - 1)) y ) b 


 






Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)]