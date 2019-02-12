import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)




-- E    X   E   R   C   I   S   E   S
-- TUPLES
-- 1. Write a function which returns both the sum and the difference between the 
-- first and seccond arguments
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = ((x + y), (x - y))


-- 2. Write a function which returns a students grade from a percentage mark.
grade :: StudentMark -> Char
grade (student, mark)
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | mark >= 0  = 'F'
    | otherwise  = 'X'


-- 3. Write a function which caps the mark of a student to a maximum of 40
capMark :: StudentMark -> StudentMark
capMark (student, mark)
    | mark >= 40 = (student, 40)
    | otherwise  = (student, mark)


-- LISTS AND STRINGS
-- 4. Write a function that gives a list of the first n numbers
firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]


-- 5. Write a function that gives the first n squares
firstSquares :: Int -> [Int]
firstSquares n = [x^2 | x <- [1..n]]


-- 6. Using list comprehension, write a function which converts the letters in 
-- a string to capitals.
capitalise :: String -> String
capitalise s = [toUpper n | n <- s]


-- 7. Using list comp. write a function what strips all non-digit characters from
-- a string.
onlyDigits :: String -> String
onlyDigits s = [n | n <- s, isDigit n]


-- 8. Using capMark function and a list comp., write a function which caps all students' marks to a maximum of 40.
capMarks :: [StudentMark] -> [StudentMark]
capMarks students = [capMark stus | stus <- students]


-- 9. Using the grade function and a list comp., write a function which grades a list of students
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stus = [(fst x, grade x) | x <- stus]


-- 10. Using recursion, write a function which joins copies of a string together
--duplicate :: String -> Int -> String
duplicate _ 0 = [] 
duplicate str n = str ++ duplicate str (n - 1)


-- 11. Using a list comp., write a function which returns the list of the divisors
-- of a positive integer.
divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], x `mod` i == 0]


-- 12. Using your divisors function, write a function which tests wether an integer
-- is prime.
isPrime :: Int -> Bool
isPrime x 
    | length (divisors x) == 2 = True
    | otherwise                = False

-- bonus: function to list the primes upto n
primesUpto :: Int -> [Int]
primesUpto n = [x | x <- [1..n], isPrime x]


-- 13. Using list comprehensions, write a polymorphic function which transforms
-- a list of pairs (of any type) into a pair of lists.
split :: [(a,b)] -> ([a],[b])
split pairList = (list1, list2)
    where
    list1 = [fst x | x <- pairList]
    list2 = [snd x | x <- pairList]


