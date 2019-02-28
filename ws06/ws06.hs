-- REMAINING QUESTIONS: 11
{- Week6.hs
 This module illustrates the use of functions as values
-}
 
import Data.Char
 
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)
 
multiply :: Int -> Int -> Int
multiply x y = x * y
 
double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit
 
keepPositive = filter (>0)
keepDigits = filter isDigit

addUp :: (Num a) => [a] -> a
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- E   X   E   R   C   I   S   E   S
-- USE OF MAP, FILTER AND FOLDR
-- 1. Write a function, mult10, that multiplies each element
-- of a list by 10. 
mult10 :: (Num a) => [a] -> [a]
mult10 = map (*10)

-- 2. Write a function, onlyLowerCase, which removes any 
-- character from a string that is no lower-case.
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- 3. Write a function, orAll, which finds the disjunction
-- of the elements in a Boolean list.
orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- 4. Write a function, sumSquares, that returns the sum of
-- the squares of the elements in a list.
sumSquares :: (Num a) => [a] -> a
sumSquares = foldr (+) 0 . map (^2) 

-- 5. Write a function, zeroToTen, that keeps only values
-- between 0 and 10 from a list.
zeroToTen :: [Integer] -> [Integer]
zeroToTen = filter (<11) . filter (>=0)

-- 6. Write a function, squareRoots, that finds the square 
-- roots of all the non-negative values in a list
squareRoots :: [Double] -> [Double] 
squareRoots = map sqrt . keepPositive

-- 7. Write a function, countBetween, that counts the
-- number of items in a list between lower and upper bounds
--countBetween ::
countBetween :: Ord a => a -> a -> [a] -> Int
countBetween lb ub = length . filter (>=lb) . filter (<=ub)

-- 8. Write a function, alwaysPositive, that tests whether
-- applying a given function to all elements of a list
-- results in only positive values.
alwaysPositive :: (Ord a1, Num a1) => (a2 -> a1) -> [a2] -> Bool
alwaysPositive f = all (>0) . map f
-- how else to do this ???

-- 9. Write a function, productSquareRoots, that finds the
-- product of the square roots of all the non-negative values in a list.
productSquareRoots :: [Double] -> Double
productSquareRoots = foldr (*) 1 . map sqrt . filter (>=0)

-- OTHER HIGHER-ORDER FUNCTIONS
-- Complete without using map, filter or foldr
-- 10. Write a function, removeFirst, that removes the first
-- element of a list that has a given property
--removeFirst 
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f (x:xs) = if f x then xs else x:removeFirst f xs

-- 11. Write a function, removeLast, that removes the last
-- element of a list that has a given property.
--removeLast f x =  

-- USING LAMBDA EXPRESSIONS
-- 12. Using filter and a single lambda expression, give an
-- alternative solution to Ex. 5
altZeroToTen :: [Integer] -> [Integer]
altZeroToTen = filter (\x -> x >= 0 && x < 11)

-- 13. Using only lambda expressions and foldr, write new
-- versions (i) the mult10 function from Ex. 1, (ii) 
-- reverse (to reverse a list), and (iii) onlyLowerCase 
-- from Ex. 2.
-- (i) mult10
altMult10 :: [Integer] -> [Integer] 
altMult10 = (\(x:xs) -> if xs == [] then x * 10 : xs else x*10 : altMult10 xs)

-- (ii) reverse
myReverse :: Foldable t => t a -> [a]
myReverse y = foldr (\x xs -> xs ++ [x]) [] y

-- (iii) onlyLowerCase
altOnlyLowerCase :: String -> String
altOnlyLowerCase = (\(x:xs) -> if isLower x then x : onlyLowerCase xs else onlyLowerCase xs)
