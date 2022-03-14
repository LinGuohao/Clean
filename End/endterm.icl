module endterm
import StdEnv

/*
My friends and I went to play football in the streets, and the game ended in a tie,
so we were discussing if we should go for penalties or not, please help me decide that.
You will get each one of my team skill Level and name in a list,
and you will get the name of the other team's goalkeeper and his/her level of skill.
If the skill of the player is more or equal than the skill of the goalkeeper
it will count as scored penalty, because my team's goalkeeper is super skillfull,
we should score 3 penalties or more to win this virtual game.
*/
::Player = { name :: String, skillLevel :: Int}

shouldWePlay :: [Player] Player -> Bool
shouldWePlay x y 
|length[z\\z<-(f1 x y)| z == True] <3 = False
=True

f1 :: [Player] Player -> [Bool]
f1 x y = [judge (z.skillLevel) (y.skillLevel) \\z <- x]

judge :: Int Int -> Bool
judge x y 
|x>=y = True
= False
//Start = shouldWePlay [{name = "kareem", skillLevel = 4},{name = "Tarek", skillLevel = 3},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // False
//Start = shouldWePlay [{name = "kareem", skillLevel = 5},{name = "Tarek", skillLevel = 4},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // True


/*
A teacher in a high school is trying to choose the student of the year in his school,
so let's help him doing that. The teacher wants the student to have a higher gpa.
The teacher is adding 0.1 to the GPA if the student was good.
*/
::Status = Good | Bad
::Pupil = {student_name :: String, gpa :: Real, status :: Status}

instance == Status
where    
         == Good Good = True
         == _ _ =False 
bestStudent :: [Pupil] -> Pupil
bestStudent x = f2h2(hd[  z\\z <- x   |toInt(z.gpa) == f2h (f2 x) && z.status == Good])

/*f3h2 :: Pupil -> Pupil
f3h2 x 
|x.gpa < 0.3 = {Pupil & x.gpa = x.gpa + 0.1}
*/
f2 :: [Pupil] -> [Pupil]
f2 x = [z\\ z <- x | z.status == Good] 

f2h:: [Pupil] -> Int
f2h x = hd (sort[toInt(z.gpa)\\ z <- x])

f2h2 :: Pupil -> Pupil
f2h2 x 
|x.gpa < 4.0 = {x & gpa = x.gpa + 0.1}
//Start = bestStudent [{student_name = "Khalid Walid", gpa = 4.0, status = Good},{student_name = "Peter", gpa = 3.7, status = Good},{student_name = "Yoko", gpa = 2.9, status = Bad}]// {student_name = "Khalid Walid", gpa = 4.0, status = Good}
//Start = bestStudent [{student_name = "Khalid Walid", gpa = 3.7, status = Bad},{student_name = "Peter", gpa = 3.7, status = Good},{student_name = "Yoko", gpa = 2.9, status = Bad}] // {student_name = "Peter", gpa = 3.8, status = Good}


/*
Given an array find the maximum value and return new array which has all occurrences of 
the maximum value removed.
For example, if given array is {1,4,5,3,3,2,4,5,1,3,4}, maximum is 5,
so answer should be {1,4,3,3,2,4,1,3,4}.
*/
remMax :: {Int} -> {Int}
remMax x  =  {m\\m <-: x|m <> last(sort[z \\ z <-: x])}



//Start = remMax {1,4,5,3,3,2,4,5,1,3,4} // {1,4,3,3,2,4,1,3,4}
//Start = remMax {1,42,42,52,452,4} // {1,42,42,52,4}
//Start = remMax {5} // {}
//Start = remMax {} // {}


/*
Given a list of Integer arrays, your task is to sum up all of them and return 
a new array. (Sum first elements of arrays, second elements of arrays and so on).
You can assume that all arrays have the same length.
*/

instance + {Int}
where    + x y = {z + m\\ z<-:x & m <-: y}

instance zero {Int}
where    zero = {}
sumArrays :: [{Int}] -> {Int}
sumArrays [] = {}
sumArrays x 
|length x == 1 = hd x 
= hd x + sumArrays (drop 1 x) 


//Start :: {Int}
//Start = {1,2,3} + {4,5,6}
//Start = sumArrays [{1,2,3}, {1,2,3}] // {2,4,6}
//Start = sumArrays [{}, {}] // {}
//Start = sumArrays [{1}, {5}] // {6}
//Start = sumArrays [{1,0,0}, {0,1,0}, {0,0,1}] // {1,1,1}


::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={tname::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}
ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}
Peter::Teacher
Peter={tname="Peter",subject="Functional"}
Viktor::Teacher
Viktor={tname="Viktor",subject="Math"}
Mary::Teacher
Mary={tname="Mary",subject="OOP"}
John::Teacher
John={tname="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}

/*
Given a University, return an array of all the 
students names which have gpa greater than 4,
and a favorite teacher who teaches Functional
*/


fun :: [Student] -> {String}
fun x = {z.studentName\\ z <- x | (z.favoriteTeacher).subject == "Functional"}


find :: University -> [Student]
find  x = [z\\ z <- x.students | f5g z > 4.0]

gpaAndFavoriteTeacher::University->{String}
gpaAndFavoriteTeacher x = fun (find x)



//Start= gpaAndFavoriteTeacher  BMI//{"Josh","Sofi"}
//Start=gpaAndFavoriteTeacher ELTE//{"Josh"}
//Start=gpaAndFavoriteTeacher EmptyUni//{}


/*
Write an instance of type Student, such that two students are equal
if their gpa differs in less than 0.3 and they have the same favorite teacher
*/

instance == Student
where    == a b = f5 a b

f5g :: Student -> Real
f5g a = toReal(sum[z\\z<-: a.grades]) /toReal(length[z\\z<-: a.grades])

f5 :: Student Student -> Bool
f5 x y
|(abs(f5g x - f5g y)) < 0.3 && (x.favoriteTeacher).tname == (y.favoriteTeacher).tname = True
=False
//Start= Nikola == Nik//True
//Start=Nikola == Nikola //True
//Start= Nik== Nik2//False
//Start= Nikola == Nik2//False


/*
Create an * instance of lists such that list1 * list2 will give a
list of pairwise product of the two lists and if the length 
of one list is greater than the other one just add the
remaining elements to the end of the new list
*/
flist :: [a] [a] -> [a] | * a
flist [] [] = []
flist x y 
|(length x < length y) = [ z * m \\ z <- x & m <- y] ++ drop (length x) y
|(length x > length y) = [ z * m \\ z <- x & m <- y] ++ drop (length y) x

instance * [a] | * a
where * x y = flist x y
 
      

//Start=[1,2]*[3,4,5,6,0]//[3,8,5,6,0]
//Start= [1,2,3,1,3,12,312] *[2,3]//[2,6,3,1,3,12,312]
//Start :: [Int]
//Start= [] * []//[]

:: Gender = Male | Female | Nghia | AttackHelicopter | OOBLECK
:: Person = {givenName :: String, lastName :: String, gender :: Gender}
:: FamilyTree = Name Person FamilyTree FamilyTree | End | Polygamy [[[[[[[[[[[FamilyTree]]]]]]]]]]]

instance == Gender
where
    == Male Male = True
    == Female Female = True
    == Nghia Nghia = True
    == AttackHelicopter AttackHelicopter = True
    == OOBLECK OOBLECK = True
    == _ _ = False

instance == Person
where
    == p1 p2 = and[p1.givenName == p2.givenName, p1.lastName == p2.lastName, p1.gender == p2.gender]

instance == FamilyTree
where
    == End End = True
    == (Name x1 l1 r1) (Name x2 l2 r2) = and[ x1==x2, l1==l2, r1==r2]
    == _ _ = False

Pedro :: Person
Pedro = {givenName = "Pedro Henrique", lastName = "Villar deFigueiredo", gender = Male}
Mauro :: Person
Mauro = {givenName = "Mauro", lastName = "daRocha Carvalho", gender = Male}
Joao :: Person
Joao = {givenName = "Joao", lastName = "Pereira Cavalcanti", gender = Male}
Carlos :: Person
Carlos = {givenName = "Carlos", lastName = "Teixeira deAndrade", gender = Male}
Luiz :: Person
Luiz = {givenName = "Luiz", lastName = "Barroso Mourao", gender = Male}
Leoberto :: Person
Leoberto = {givenName = "Leoberto", lastName = "Praxedes Santos", gender = Male}
Luan :: Person
Luan = {givenName = "Luan", lastName = "deRosas Lima", gender = Male}
Matheus :: Person
Matheus = {givenName = "Matheus", lastName = "Andrade Duarte", gender = Male}
AnaMaria :: Person
AnaMaria = {givenName = "Ana Maria", lastName = "Silva Figueira", gender = Female}
Lucia :: Person
Lucia = {givenName = "Lucia", lastName = "Elena Paiva", gender = Female}
Elena :: Person
Elena = {givenName = "Elena Maria", lastName = "Lacerda Leite", gender = Female}
Vitoria :: Person
Vitoria = {givenName = "Vitoria", lastName = "Correia Negrao", gender = Female}
Miriam :: Person
Miriam = {givenName = "Miriam", lastName = "Marinho Silva", gender = Female}
Veronica :: Person
Veronica = {givenName = "Veronica", lastName = "Soares deCarvalho", gender = Female}
Olivia :: Person
Olivia = {givenName = "Olivia", lastName = "Alves daSilva", gender = Female}
Maria :: Person
Maria = {givenName = "Maria Luiza", lastName = "Gama Pordeus", gender = Female}
Bruna :: Person
Bruna = {givenName = "Bruna", lastName = "Melo Guedes", gender = Female}

PedroFamily :: FamilyTree
PedroFamily = Name Pedro MauroFamily LuciaFamily

MauroFamily :: FamilyTree
MauroFamily = Name Mauro JoaoFamily ElenaFamily
LuciaFamily :: FamilyTree
LuciaFamily = Name Lucia CarlosFamily VitoriaFamily

JoaoFamily :: FamilyTree
JoaoFamily = Name Joao LuizFamily MiriamFamily
ElenaFamily :: FamilyTree
ElenaFamily = Name Elena LeobertoFamily VeronicaFamily
CarlosFamily :: FamilyTree
CarlosFamily = Name Carlos LuanFamily OliviaFamily
VitoriaFamily :: FamilyTree
VitoriaFamily = Name Vitoria MatheusFamily MariaFamily

LuizFamily :: FamilyTree
LuizFamily = Name Luiz End End
MiriamFamily :: FamilyTree
MiriamFamily = Name Miriam End End
LeobertoFamily :: FamilyTree
LeobertoFamily = Name Leoberto End End
VeronicaFamily :: FamilyTree
VeronicaFamily = Name Veronica End End
LuanFamily :: FamilyTree
LuanFamily = Name Luan End End
OliviaFamily :: FamilyTree
OliviaFamily = Name Olivia End End
MatheusFamily :: FamilyTree
MatheusFamily = Name Matheus End End
MariaFamily :: FamilyTree
MariaFamily = Name Maria End End

fixedTree :: FamilyTree
fixedTree = (Name {givenName = "Pedro Henrique", lastName = "Lima Mourao", gender = Male} (Name {givenName = "Mauro", lastName = "Santos Mourao", gender = Male} (Name {givenName = "Joao", lastName = "Silva Mourao", gender = Male} (Name {givenName = "Luiz", lastName = "Barroso Mourao", gender = Male} End End )(Name {givenName = "Miriam", lastName = "Marinho Silva", gender = Female} End End ))(Name {givenName = "Elena Maria", lastName = "deCarvalho Santos", gender = Female} (Name {givenName = "Leoberto", lastName = "Praxedes Santos", gender = Male} End End )(Name {givenName = "Veronica", lastName = "Soares deCarvalho", gender = Female} End End )))(Name {givenName = "Lucia", lastName = "Duarte Lima", gender = Female} (Name {givenName = "Carlos", lastName = "daSilva Lima", gender = Male} (Name {givenName = "Luan", lastName = "deRosas Lima", gender = Male} End End )(Name {givenName = "Olivia", lastName = "Alves daSilva", gender = Female} End End ))(Name {givenName = "Vitoria", lastName = "Pordeus Duarte", gender = Female} (Name {givenName = "Matheus", lastName = "Andrade Duarte", gender = Male} End End )(Name {givenName = "Maria Luiza", lastName = "Gama Pordeus", gender = Female} End End ))))

/*
Would you kindly write a function that takes
a FamilyTree and returns the FamilyTree with 
the last names of everyone fixed.


In a FamilyTree, the root is the child,
and the left and right nodes are the parents.

Everyone here is Brazilian.
Each person's last name has two parts.
The first part comes from the mother's last name's second part,
the second part comes from the father last name's second part.

For example, if the parents are:
Mother -> Olivia Alves daSilva
Father -> Mauro daRocha Carvalho
Child -> Pedro daSilva Carvalho

If the Child has no parents, then do not fix their last name.
For simplicity sake, assume every child will have 2 Parents or none.
*/
//fixLastNames :: FamilyTree -> FamilyTree
//Start = fixLastNames PedroFamily == fixedTree //True

:: Router = { nodeName :: String, activeStatus :: Bool}
:: Network = Node Router Network Network | Termination

r1 :: Router
r1 = {nodeName = "PL1", activeStatus = True}
r2 :: Router
r2 = {nodeName = "PL2", activeStatus = True}
r3 :: Router
r3 = {nodeName = "PL3", activeStatus = False}
r4 :: Router
r4 = {nodeName = "PL4", activeStatus = True}
r5 :: Router
r5 = {nodeName = "PL5", activeStatus = False}
r6 :: Router
r6 = {nodeName = "PL6", activeStatus = True}
r7 :: Router
r7 = {nodeName = "PL7", activeStatus = True}
r8 :: Router
r8 = {nodeName = "PL8", activeStatus = False}
r9 :: Router
r9 = {nodeName = "PL9", activeStatus = False}
r10 :: Router
r10 = {nodeName = "PL10",  activeStatus = False}
r11 :: Router
r11 = {nodeName = "PL11",  activeStatus = True}
r12 :: Router
r12 = {nodeName = "PL12",  activeStatus = True}
r13 :: Router
r13 = {nodeName = "PL13",  activeStatus = False}
r14 :: Router
r14 = {nodeName = "PL14",  activeStatus = True}
r15 :: Router
r15 = {nodeName = "PL15",  activeStatus = True}
r16 :: Router
r16 = {nodeName = "PL16",  activeStatus = True}
r17 :: Router
r17 = {nodeName = "PL17",  activeStatus = True}
r18 :: Router
r18 = {nodeName = "PL18",  activeStatus = False}
r19 :: Router
r19 = {nodeName = "PL19",  activeStatus = True}
r20 :: Router
r20 = {nodeName = "PL20",  activeStatus = False}
r21 :: Router
r21 = {nodeName = "PL21",  activeStatus = False}
r22 :: Router
r22 = {nodeName = "PL22",  activeStatus = True}

noNetwork :: Network
noNetwork = Termination
oneNetwork :: Network
oneNetwork = Node r1 Termination Termination
smolNetwork :: Network
smolNetwork = Node r1 (Node r2 (Node r4 Termination Termination)(Node r5 Termination Termination))(Node r3 (Node r6 Termination Termination) (Node r7 Termination Termination))
bigNetwork :: Network
bigNetwork = Node r11 (Node r4 (Node r3 (Node r1 Termination Termination) Termination) (Node r6 (Node r5 Termination Termination) Termination)) (Node r16 (Node r14 Termination (Node r15 Termination Termination)) (Node r19 (Node r18 Termination Termination) (Node r21 (Node r20 Termination Termination) (Node r22 Termination Termination))))

/*
Would you kindly write a function, that takes a Network
and a name of a Router, and returns a Bool indicating
if that Router is sucessfully connected.

A Router is successfully connected if ALL NODES
from the root to the router node have activeStatus of True.

If the Router is not found, return False.
*/
//pathCheck :: Network String -> Bool



//Start = pathCheck smolNetwork "PL4" //True
//Start = pathCheck smolNetwork "PL69" //False
//Start = pathCheck bigNetwork "PL15" //True
//Start = pathCheck bigNetwork "PL22" //False
//Start = pathCheck oneNetwork "PL1" //True
//Start = pathCheck noNetwork "HelloGoodBye" //False


/*
    Create a class called Comparisons and define the operations binary operations:
     *== , != , *< , *> , *<= ,*>= 
    Given two elements it compares them and gives out a boolean.
    Example: x *== y should check if the x and y are equal.
    != -> not equal
    *< -> less (smaller)
    *> -> greater (bigger)
    *<= -> less (smaller) or equal
    *>= -> greater (bigger) or equal
    )
    Make an instance for integers. 
*/
class Comparisons a
where 
      (*==) :: a a -> Bool
      (!=):: a a -> Bool
      (*<):: a a -> Bool
      (*>):: a a -> Bool
      (*<=):: a a -> Bool
      (*>=):: a a -> Bool

instance Comparisons Int
where    
         (*==) x y
         |x <> y = False
         =True
         (!=) x y
         |x <> y =True
         =False
         (*<) x y
         |x < y =True
         =False
         (*>)x y
         |x > y = True
         =False
         (*<=) x y
         |x <= y = True
         =False
         (*>=) x y
         |x >= y = True
         =False
           
        
//Start = 3 *== 3 //True
//Start = 3!=3//False
//Start = 3*<5//True