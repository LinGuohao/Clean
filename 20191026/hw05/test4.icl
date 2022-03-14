module test4
import StdEnv


:: Vector3 a = {x0 :: a, x1 :: a, x2 :: a}

/*
Implement instances : zero, one, ~, +, -, *, /  for the Vector3 type( you can refer to Vector2)
 e.g. instance ==   (Vector3 a) | == a   where == vector0 vector1	= vector0.x0 == vector1.x0 && vector0.x1 == vector1.x1 && vector0.x2 == vector1.x2
*/
instance ==   (Vector3 a) | == a where == vector0 vector1 = vector0.x0 ==vector1.x0 && vector0.x1 == vector1.x1 && vector1.x2 == vector1.x2
instance zero (Vector3 a) | zero a where zero = {x0 = zero ,x1 = zero,x2 = zero}
instance one  (Vector3 a) | one a where one = {x0 = one , x1 = one ,x2 = one}
instance ~    (Vector3 a) | ~ a where ~ vector0 = {x0 = ~vector0.x0 , x1 = ~vector0.x1 , x2 = ~vector0.x2}
instance +    (Vector3 a) | + a where + vector0 vector1 = {x0 = (vector0.x0 + vector1.x0), x1 =(vector0.x1 + vector1.x1), x2 = (vector0.x2 + vector1.x2)}
instance -    (Vector3 a) | - a where - vector0 vector1 =  {x0 = (vector0.x0 - vector1.x0), x1 = (vector0.x1 - vector1.x1),x2 = (vector0.x2 - vector1.x2)}
instance *    (Vector3 a) | * a where * vector0 vector1	= {x0 = (vector0.x0 * vector1.x0), x1 = (vector0.x1 * vector1.x1), x2 = (vector0.x2 * vector1.x2)}

instance /    (Vector3 a) | / a where / vector0 vector1	= {x0 = (vector0.x0 / vector1.x0), x1 = (vector0.x1 / vector1.x1),x2 = (vector0.x2 / vector1.x2)}

Start	= {x0 = 5, x1 = 6 , x2 = 7} + one //(Vector3 6 7 8)
