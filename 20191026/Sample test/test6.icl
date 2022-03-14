module test6
import StdEnv

/*6
// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
// the second part contains the rest. */
*/
TwoLists :: [Char] -> ([Char], [Char])
TwoLists [] = ([],[])
TwoLists n = (list1 n, list2 n)



list1 :: [Char] -> [Char]
list1 [] = []
list1 [a:b] = [a] ++ list1 (drop 2 [a:b])


list2 :: [Char] -> [Char]
list2 [] = []
list2 n = take 1 (drop 1 n) ++ list2 (drop 2 n) 
//Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

Start = TwoLists [] // ([],[])