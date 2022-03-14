module hw10
import StdEnv

//Given these Algebraic Data Types, Records, and Tree...
:: Gender = Male | Female | NonBinary | AttackHelicopter | Nghia | OOBLECK
:: LivingStatus = Alive | Deceased | Undead
:: MarriageStatus = Married | Divorced | Single | Tinder
:: Person = { name :: String, gender :: Gender, age :: Int, livingStatus :: LivingStatus, marriageStatus :: MarriageStatus}
:: FamilyTree a = Name a (FamilyTree a) (FamilyTree a) | End | Polygamy [[[[[[[[[[[[[FamilyTree a]]]]]]]]]]]]]

//And these people:
Olivia = {name = "Olivia", gender = Female, age = 19, livingStatus = Alive, marriageStatus = Single}
Amelia = {name = "Amelia", gender = Female, age = 83, livingStatus = Alive, marriageStatus = Married}
Isla = {name = "Isla", gender = Female, age = 40, livingStatus = Alive, marriageStatus = Married}
Emily = {name = "Emily", gender = Female, age = 73, livingStatus = Alive, marriageStatus = Divorced}
Ava = {name = "Ava", gender = Female, age = 18, livingStatus = Alive, marriageStatus = Single}
Lily = {name = "Lily", gender = Female, age = 50, livingStatus = Alive, marriageStatus = Divorced}
Oliver = {name = "Oliver", gender = Male, age = 56, livingStatus = Alive, marriageStatus = Married}
Harry = {name = "Harry", gender = Male, age = 45, livingStatus = Alive, marriageStatus = Married}
Jack = {name = "Jack", gender = Male, age = 90, livingStatus = Deceased, marriageStatus = Married}
George = {name = "George", gender = Male, age = 43, livingStatus = Alive, marriageStatus = Married}
Noah = {name = "Noah", gender = Male, age = 74, livingStatus = Undead, marriageStatus = Divorced}
Freddie = {name = "Freddie", gender = Male, age = 24, livingStatus = Alive, marriageStatus = Single}
Ethan = {name = "Ethan", gender = Male, age = 20, livingStatus = Alive, marriageStatus = Single}

//And each person's immediate parents:
OliviaTree = Name Olivia OliverTree HarryTree
OliverTree = Name Oliver End End
HarryTree = Name Harry AmeliaTree JackTree
AmeliaTree = Name Amelia End End
JackTree = Name Jack End End
EthanTree = Name Ethan GeorgeTree IslaTree
GeorgeTree = Name George AmeliaTree JackTree
IslaTree = Name Isla NoahTree EmilyTree
NoahTree = Name Noah End End
EmilyTree = Name Emily End End
AvaTree = Name Ava LilyTree OliverTree
LilyTree = Name Lily End End
FreddieTree = Name Freddie End End

personsList = [Olivia, Amelia, Isla, Emily, Ava, Lily, Oliver, Harry, Jack, George, Noah, Freddie, Ethan]
familyList = [OliviaTree, OliverTree, HarryTree, AmeliaTree, JackTree, EthanTree, GeorgeTree, IslaTree, NoahTree, EmilyTree, AvaTree, LilyTree, FreddieTree]


/*
Write a function that tests if two persons are cousins
Condition: They share a grandparent.
*/

instance == Person
where
     (==) a b = a.name == b.name
           
instance == (FamilyTree Person)
where
              (==) End End = True
              (==) _ _ = False

return :: Person [FamilyTree Person] -> [FamilyTree Person]
return  a x =([(Name m l r)\\(Name m l r) <- x | m.name == a.name])


list :: [FamilyTree Person] -> [(FamilyTree Person)]
list x = [l\\ (Name m l r) <- x] ++ [r\\ (Name m l r) <- x]

list2 :: [(FamilyTree Person)] -> [(FamilyTree Person)]
list2 x = [l \\(Name m l r)<- x] ++ [r\\(Name m l r) <- x]

parents :: [(FamilyTree Person)] -> [String]
parents [] = []
parents [x:xs]
|x == End = [] ++ parents xs  
= get x ++ parents xs

where 
   get (Name x l r) = [x.name]


judge :: [String] [String]  -> [Bool]
judge [] _ = []
judge [x:xs] y 
|isMember x y = [True] ++ judge xs y 
= [False] ++ judge xs y

areCousins :: Person Person -> Bool
areCousins x y 
|x.name == y.name = False
= or(judge ( parents (list2 (list (return x familyList))))( parents (list2 (list (return y familyList)))))
 
 



//Start = parents (list2 (list (return Harry familyList)))
//Start = areCousins Ethan Olivia //True
//Start = parents (list2 (list (return Olivia familyList)))
//Start = areCousins Ethan Ava //False
//Start = areCousins George Harry //False
//Start = areCousins George Isla //False
//Start = areCousins Ethan Ethan //False (same person)


:: IpV4Address :== (Int,Int,Int,Int)
:: Router = { nodeName :: String, ipAddress :: IpV4Address, activeStatus :: Bool}
:: Availability = NodeUp | NodeDown
:: Status = OK (Availability,Router) | NOK
instance == Availability
where
    == NodeUp NodeUp = True
    == NodeDown NodeDown = True
    == _ _ = False
instance toString Status
where
    toString NOK = "\nNo Router Match.\n"
    toString (OK (a,{nodeName = n, ipAddress = (i1,i2,i3,i4)}))
    |  a == NodeUp = stdOutput+++"Status: Available\n"
    = stdOutput +++ "Status: Unavailable\n"
    
    where
        tab = "    "
        stdOutput = "\nRouter Match:\n"+++tab+++"Router Name: "+++n+++"\n"+++tab+++"IpV4 Address: "+++toString i1+++"."+++toString i2+++"."+++toString i3+++"."+++toString i4+++"\n"+++tab

r1 :: Router
r1 = {nodeName = "PL1", ipAddress = (10,0,0,1), activeStatus = True}
r2 :: Router
r2 = {nodeName = "PL2", ipAddress = (10,0,0,2), activeStatus = True}
r3 :: Router
r3 = {nodeName = "PL3", ipAddress = (10,0,0,3), activeStatus = False}
r4 :: Router
r4 = {nodeName = "PL4", ipAddress = (10,0,0,4), activeStatus = True}
r5 :: Router
r5 = {nodeName = "PL5", ipAddress = (10,0,0,5), activeStatus = False}

CurrentRouters :: [Router]
CurrentRouters = [r1,r2,r3,r4,r5]

/*
Write a class of functions that will return a Router's status.
The function should be able to take a nodeName in the form
of a String, or an IpV4 address in the form of the predefined
type IpV4Address and return a Status, provided a list of existing routers.
The Status should be NOK if there is no matching Router.
The Status should be OK if there is a matching Router.
Availability will be NodeUp if the activeStatus is True.
Otherwise Availability will be NodeDown.

Note: You don't need to touch any of the definitions above, 
those have been written for you and will work provided that
your getStatus works properly.
*/

giveact :: Bool -> Availability
giveact True = NodeUp
giveact False = NodeDown

f1 :: String [Router] -> Status
f1 x y 
|isMember x [z.nodeName \\ z <- y] = OK (giveact (hd(filter (getname x) y)).activeStatus, (hd(filter (getname x) y))   )
= NOK
where getname x y 
             |x == y.nodeName = True
             |x <> y.nodeName = False 

f2::  IpV4Address [Router] -> Status
f2 x y 
|isMember x [z.ipAddress \\ z <- y] = OK (giveact (hd(filter (getip x) y)).activeStatus, (hd(filter (getip x) y))   )
= NOK

where getip x y
       |x == y.ipAddress = True
       |x<> y.ipAddress  = False


instance == IpV4Address 
where == (a,b,c,d) (q,w,e,r) 
          |(a == q && b == w && c == e && d == r) = True
          = False
         

 
class getStatus a b :: a b-> Status

instance getStatus String [Router]
where 
         getStatus  x y = f1 x y
instance getStatus IpV4Address [Router]
where      
         getStatus x y = f2 x y
//Start = toString(getStatus "PL1" CurrentRouters)
/*
"
Router Match:
    Router Name: PL1
    IpV4 Address: 10.0.0.1
    Status: Available
"
*/

//Start = toString(getStatus (10,0,0,5) CurrentRouters)
/*
"
Router Match:
    Router Name: PL5
    IpV4 Address: 10.0.0.5
    Status: Unavailable
"
*/

Start = toString(getStatus "NULL_ROUTER" CurrentRouters)
/*
"
No Router Match.
"
*/
