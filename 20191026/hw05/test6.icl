module test6
import StdEnv

:: Vector3 a = {x0 :: a, x1 :: a, x2 :: a}

instance ==   (Vector3 a) | == a where == vector0 vector1 = vector0.x0 ==vector1.x0 && vector0.x1 == vector1.x1 && vector1.x2 == vector1.x2
instance zero (Vector3 a) | zero a where zero = {x0 = zero ,x1 = zero,x2 = zero}
instance +    (Vector3 a) | + a where + vector0 vector1 = {x0 = (vector0.x0 + vector1.x0), x1 =(vector0.x1 + vector1.x1), x2 = (vector0.x2 + vector1.x2)}
instance one  (Vector3 a) | one a where one = {x0 = one , x1 = one ,x2 = one}
instance ~    (Vector3 a) | ~ a where ~ vector0 = {x0 = ~vector0.x0 , x1 = ~vector0.x1 , x2 = ~vector0.x2}
instance -    (Vector3 a) | - a where - vector0 vector1 =  {x0 = (vector0.x0 - vector1.x0), x1 = (vector0.x1 - vector1.x1),x2 = (vector0.x2 - vector1.x2)}
instance *    (Vector3 a) | * a where * vector0 vector1	= {x0 = (vector0.x0 * vector1.x0), x1 = (vector0.x1 * vector1.x1), x2 = (vector0.x2 * vector1.x2)}
instance /    (Vector3 a) | / a where / vector0 vector1	= {x0 = (vector0.x0 / vector1.x0), x1 = (vector0.x1 / vector1.x1),x2 = (vector0.x2 / vector1.x2)}


/*
Use mathematical knowledge related to vector cross product to implement the following function:
*/
Vec3crossProduct ::  (Vector3 a) (Vector3 a) ->  (Vector3 a) | *,-a
Vec3crossProduct vec1 vec2 = {x0 = ((vec1.x1 * vec2.x2) - (vec1.x2 * vec2.x1)) , x1 = ((vec1.x2 * vec2.x0) - (vec1.x0 * vec2.x2)), x2 = ((vec1.x0 * vec2.x1) - (vec1.x1 * vec2.x0))}


Start = Vec3crossProduct {x0 = 1.5, x1 = 2.6, x2 = 3.0} {x0 = 5.0, x1 = 2.6, x2 = 4.5} //(Vector3 3.9 8.25 -9.1)