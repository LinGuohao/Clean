module test1
import StdEnv


:: Q = {num1 :: Int, num2 :: Int}

f :: Q -> Q
f n = {num1 = n.num1 / (gcd(n.num1)(n.num2)), num2 = n.num2/(gcd(n.num1)(n.num2))}


Start = f {num1 = 15 , num2 = 20}