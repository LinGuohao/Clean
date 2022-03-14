module classwork07
import StdEnv

// Given three points, represented as Vector2 Record, write a function which decides if a they form a
// Equilateral Triangle (triangle which has three equal sides).

::Vector2 = {x :: Real , y :: Real }


isEquilateral :: Vector2 Vector2 Vector2 -> Bool
isEquilateral a b c
|((sqrt(a.x - b.x)^2.0 + (a.y - b.y)^2.0) == (sqrt(a.x - c.x)^2.0 + (a.y - c.y)^2.0) &&  (sqrt(a.x - b.x)^2.0 + (a.y - b.y)^2.0) == (sqrt(b.x - c.x)^2.0 + (b.y - c.y)^2.0)) = True
= False


//Start = isEquilateral {x=(-4.0) , y=(-2.0)} {x=(-3.0) , y=(7.0)} {x=(4.0) , y=(-2.0)}//False
//Start = isEquilateral {x=(0.0) , y=(0.0)} {x=(4.0) , y=(0.0)} {x=(2.0) , y=(sqrt(3.0)*2.0)} //True
//Start = isEquilateral {x=(-3.0) , y=(2.0)} {x=(2.0) , y=(1.0)} {x=(-2.0) , y=(-3.0)}//False




// Given two lists of triples, merge the lists together such that the third elements
// are on an increasing, provided that lists are already sorted by third element.
Merge:: [(Int,Int,Int)] [(Int,Int,Int)]-> [(Int,Int,Int)]
Merge [] ys = ys
Merge xs [] = xs 
Merge [x:xs] [y:ys]
|thd3 x <= thd3 y = [x: merge xs [y:ys] ]
= [y: merge [x:xs] ys]


 // [(1,1,1),(2,2,2),(1,2,3),(4,5,6),(7,8,9)]
//Start = Merge [(32,1,4),(3,21,5)] [(22,90,54)] // [(32,1,4),(3,21,5),(22,90,54)]
//Start = Merge [(1,1,1),(3,3,3),(5,5,5)] [(2,2,2),(4,4,4)] // [(1,1,1),(2,2,2),(3,3,3),(4,4,4),(5,5,5)]
//Start = Merge [] [(1,1,1)] // [(1,1,1)]
// Start = Merge [] [] //[]



// Give a list of people of record type Person.
// Find count of t he most popular programming language among Females.
// The gender in record Person must be Algebraic type: Gender
:: Gender = Male | Female 
:: person = {name :: String, gender :: Gender, language :: String }

isFemale :: Gender -> Bool
isFemale Female = True
isFemale _ = False  



mostPopular :: [person] -> Int
mostPopular n = [x.language\\ x <- n | isFemale (x.gender)]
// Start = mostPopular [{name = "Alice", gender = Female, language="C++"},{name = "Alice2", gender = Female, language="C++"},{name = "Alice3", gender = Female, language="C++" }, {name = "Alice4", gender = Female, language="Clean"}] // 3
// Start = mostPopular [{name = "Alice", gender = Female, language="C++"},{name = "Alice2", gender = Female, language="Clean"},{name = "Alice3", gender = Male, language="Python" }, {name = "Alice4", gender = Female, language="Clean"}] // 2
// Start = mostPopular [{name = "Alice", gender = Male, language="C++"},{name = "Alice2", gender = Male, language="Clean"},{name = "Alice3", gender = Male, language="Python" }, {name = "Alice4", gender = Male, language="Clean"}] // 0