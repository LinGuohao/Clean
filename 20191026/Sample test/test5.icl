module test5
import StdEnv


/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

f :: [Int] [Int] -> Int
f [] [y:ys] = 0
f [x:xs] [] = 0 
f [x][y] = x*y
f [x:xs] [y:ys] = x*y + f xs ys




//Start = f [4,6,3] [6,3,7] //63

//Start = f [6,3,7] [4,6,3] //63

Start = f [5,2,6,8,3] [5,-8,5,-3,-5] //0