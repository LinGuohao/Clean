module cw08
import StdEnv


f1:: {Int} Int -> {Int}
f1 x y = {(z *2)\\ z <-: x | (z * 2) rem  y == 0} 



//Start = f1 {1,2,3,4} 4 //{4,8}
//Start = f1 {3,4,5,7,2,9} 3 //{6,18}

/*
Implement a function that acts as 'foldr' for
arrays.
*/
arrFold :: (a -> b -> b) b {a} -> b
arrFold x y z = foldr x y [u\\ u <-: z]

//Start = arrFold (+) 0 {1,2,3,4,5}  
//Start = arrFold (++) [] {[1],[2],[3],[4]} // [1,2,3,4]