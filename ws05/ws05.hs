{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

type StudentMark = (String, Int)
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71),
            ("Sue", 55), ("Sue", 37)]

-- E X E R C I S E S
-- LIST PATTERNS
-- 1. Write a function which returns the first element plus one for a
-- non-empty list, and 0 for an empty list.
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:xs) = 1 + x

-- 2. Write a polymorphic function which adds an extra copy of the 
-- first element at the beginning of the list (or returns [] for an
-- empty list)
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:x:xs

-- 3. Write a polymorphic function which swaps the first two elements
-- of a list (or leaves the list unchanged if it contains fewer than
-- two elements)
rotate :: [a] -> [a]
rotate (x:x2:xs) = x2:x:xs
rotate x = x -- case for empty or single item list

-- RECURSION OVER LISTS
-- 4. Write a recursive polymorphic function which returns the length 
-- of a list
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- 5. Write a recursive function which returns the product of all
-- integers in a list or 1 for an empty list
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

-- 6. Write a recursive function which returns the conjunction (and)
-- for all elements of a list. Return True for empty list.
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = if x == True then andAll (xs) else False

-- 7. Write a recursive function which counts the number of times a 
-- given value appears in a list
countElems :: Int -> [Int] -> Int
countElems _ [] = 0
countElems n (x:xs)
    | x == n = 1 + countElems n xs
    | otherwise = 0 + countElems n xs

-- 8. Write a recursive function that removes all the copies of a given
-- value from a list of integers
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x:(removeAll n xs)

-- 9. Write a recursive function which gives a list of the marks for a
-- particular student.
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks s ((n,m):xs)
    | n == s = m:(listMarks s xs)
    | otherwise = listMarks s xs

-- 10. Write a recursive function which decides if the first list is a 
-- prefix of the second list (e.g. [1,4] is a prefix of [1,4,3,2]).
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = if x == y then prefix xs ys else False

-- 11. Using the prefix function, write a recursive function which
-- decides if the first list is contained in the second (e.g. [1,4,9] is
-- a subsequence of [8,1,4,9,2,1], and [] is a subsequence of any list).
subSequence :: [Int] -> [Int] -> Bool
subSequence _ [] = False
subSequence x y 
    | (prefix x y) = True
    | otherwise = subSequence x (tail y)
