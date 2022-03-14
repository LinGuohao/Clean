module cw06g02 
import StdEnv

// 1. // It is hypothesized that every even number greater than two can be expressed as the sum of two primes. // For example, 4 = 2+2, 6 = 3+3, 8 = 3+5. // Return a tuple (number, prime1, prime2) for all even numbers in the range 4 to n that satisfy this hypothesis. // Requirement: (prime1, prime2) is the same as (prime2, prime1), so only one should be displayed // Ex: (10,3,7) ~~ (10,7,3) // Hint/Requirement: Use list comprehension
primes :: Int -> [(Int,Int,Int)]
primes n = [(x,y,z)\\ x <- [4..n] , y <- [2..n] , z <- [2..n] | isEven x && x <= n && x == (y+z) && prim y && prim z && y<=z]

prim :: Int -> Bool
prim 1 = False
prim n = [x\\ x <- [2..(n/2)]| n rem x == 0] == []
//Start = primes 10 // [(4,2,2),(6,3,3),(8,3,5)] // 
//Start = primes 15 // [(4,2,2),(6,3,3),(8,3,5),(10,3,7),(10,5,5)] // Start = primes 15 // [(4,2,2),(6,3,3),(8,3,5),(10,3,7),(10,5,5),(12,5,7),(14,3,11),(14,7,7)]




// 2. // Given three Vectors in 3D, decide if they are in a same plane. // Hint1: https://math.stackexchange.com/questions/451661/check-whether-the-three-vectors-a2-1-2-b1-2-3-c3-4-7-are-in-the-same // Hint2: Check last two tasks of your homework // Hint3: Google for ¡°vector dot product¡± and ¡°vector cross product¡± // :: Vector3 = {x :: Real, y :: Real, z :: Real} // coplanar :: Vector3 Vector3 Vector3 -> Boo ::  (Vector3 a) (Vector3 a) ->  (Vector3 a) | *,-a
:: Vector3 = {x :: Real, y :: Real, z :: Real} 
coplanar :: Vector3 Vector3 Vector3 -> Bool
coplanar v1 v2 v3
|((v1.x * v1.x + v1.y * v1.y + v1.z * v1.z) * ((v2.x * v2.x + v2.y * v2.y + v2.z * v2.z) * (v3.x * v3.x + v3.y * v3.y + v3.z * v3.z))) <> 0.0 = True
= False


//Start = Vec3crossProduct {x0 = 1.5, x1 = 2.6, x2 = 3.0} {x0 = 5.0, x1 = 2.6, x2 = 4.5} //(Vector3 3.9 8.25 -9.1)




Start = coplanar {x = 0.0, y = 1.0, z = 2.0} {x = 0.0, y = 2.0, z = 3.0} {x = 0.0, y = -1.0, z = 3.0} //