-- 1. Write an 'absolute' function using guards.
absolute :: Int -> Int
absolute x
    | x < 0 = x * (-1)
    | otherwise = x

-- 2. Write a function that returns 1 for positive values, -1 for negative values
-- and 0 for zero-valued arguments.
sign :: Int -> Int
sign x 
    | x < 0     = (-1)
    | x == 0    = 0
    | otherwise = 1 

-- 3. Write a function that determines how many of its three arguments are the 
-- same.
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z           = 3
    | x == y || y == z || z == x = 2
    | otherwise                  = 0

-- 4. Write a function which takes the side-lengths of three squares and returns
-- the sum of the lengths of their diagonals.
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagX + diagY + diagZ
                           where
                           diagX = sqrt (2) * x
                           diagY = sqrt (2) * y
                           diagZ = sqrt (2) * z

-- 5. A Taxi company calculates fares based on distance travelled. Fares start at
-- £2.20; 50p is added for each kilometer covered for the first 10 kilometres; and
-- 30p is added for each additional kilometer. Write a function which takes the
-- distance in kilometres, and returns the fare in pounds.

--taxiFare :: Int -> Float -- why does this break it???
taxiFare :: (Ord a, Fractional a) => a -> a --type signature given by :t taxiFare
taxiFare km
    | km < 10   = (2.2 + 0.5 * km)
    | otherwise = (7.2 + (km - 10) * 0.3)

-- 6. Write a function that returns the number of values above the average of the
-- three input values.
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = length aboveAve
                            where
                            vals = [fromIntegral x, fromIntegral y, fromIntegral z]
                            ave = (sum vals) / 3 
                            aboveAve = [a | a <- vals, a > ave]

-- 7. Write a function which takes integers representing a day and a month, and 
-- returns True if, and only if, the date is valid.
validDate :: Int -> Int -> Bool
validDate d m 
    | d <= 28 && d > 0 && m == 2           = True
    | d <= 30 && d > 0 && m `elem` mWith30 = True
    | d <= 31 && d > 0 && m `elem` mWith31 = True
    | otherwise                            = False
    where
    mWith30 = [4, 7, 9, 11]
    mWith31 = [1, 3, 5, 6, 8, 10, 12]

-- 8. Assuming that all years divisible by 4 are leap years, write a function which
-- returns the number of days for a given month and year
daysInMonth :: Int -> Int -> Int
daysInMonth m y
    | m `elem` mWith30       = 30
    | m `elem` mWith31       = 31
    | m == 2 && mod y 4 == 0 = 29
    | m == 2                 = 28
    | otherwise              = error "Invalid month"
    where
    mWith30 = [4, 7, 9, 11]
    mWith31 = [1, 3, 5, 6, 8, 10, 12]
