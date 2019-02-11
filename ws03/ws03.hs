import Prelude hiding ((&&), gcd)
infixr 3 &&
-- PATTERN MATCHING
-- 1. Write 3 re-implementations for the && operator.

--(&&) :: Bool -> Bool -> Bool
--True && True    = True
--False && True   = False
--True && False   = False
--False && False  = False

--(&&) :: Bool -> Bool -> Bool
--True && True    = True
--_ && _          = False

(&&) :: Bool -> Bool -> Bool
False && _      = False
True && a       = a


-- 2. Using pattern matching, define an XOR function.

exOr :: Bool -> Bool -> Bool
True `exOr` a  = not a
False `exOr` a = a


-- 3. Using pattern matching write a function which returns the seccond parameter
-- if the first parameter is True and the third paramter if the first parameter
-- is False.

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse _ _ b    = b


-- 4. Using pattern matching which takes an integer (assumed between 1 and 12)
-- and returns the number of days in the corresponding month.

daysInMonth :: Int -> Int
daysInMonth 2  = 28
daysInMonth 9  = 30 
daysInMonth 4  = 30
daysInMonth 6  = 30
daysInMonth 10 = 30
daysInMonth _  = 31


-- RECURSION
-- 5. Write a recursive function that gives the sum of the first n positive integers.

sumNumbers :: Int -> Int
sumNumbers 1 = 1
sumNumbers n = n + sumNumbers (n - 1)


-- 6. Write a recursive function that gives the sum of the first n squares.

sumSquares :: Int -> Int
sumSquares 1 = 1
sumSquares n = n^2 + sumSquares (n - 1)


-- 7. Recursive function to raise a value to the power of another.

power :: Int -> Int -> Int
power a 1 = a
power a b = a * power a (b - 1)


-- 8. Recursive function that gives the sum of integers between and including its
-- arguments.

sumFromTo :: Int -> Int -> Int
sumFromTo m n = if m == n then m else n + sumFromTo m (n - 1)


-- 9. Recursive function to find greatest common divisor (GCD) of two non-negative
-- integers.

gcd :: Int -> Int -> Int
gcd a b = if a == b then a else gcd (abs (a - b)) (min a b)


-- 10. Use recursion to find the integer square root of a value n (the largest value
-- whose square is less than or equal to n).

intSquareRoot :: Int -> Int
intSquareRoot n = head [x | x <- [n, n - 1..1], x `power` 2 <= n]
-- how could this be re-implemented using only things we have been taught in lectures?

-- taken from: https://stackoverflow.com/questions/19965149/integer-square-root-function-in-haskell/19965405
intSQRT :: Int -> Int
intSQRT n = aux n
    where
    aux x
        | x*x > n = aux (x - 1)
        | otherwise = x


-- 11. Re-implement two functions from Qs 5 - 10 using guards.

-- 6. Using guards
sumSquaresG :: Int -> Int
sumSquaresG n
    | n == 1    = 1
    | n > 1     = n^2 + sumSquaresG (n - 1)
    | otherwise = error "sumSquareG not supportive of negative values"

-- 7. Using guards
powerG :: Int -> Int -> Int
powerG a b 
    | b == 1    = a
    | otherwise = a * powerG a (b - 1) 


