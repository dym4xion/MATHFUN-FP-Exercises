-- 1. Write a function which multiplies its argument by 10
timesTen :: Int -> Int
timesTen x = 10 * x


-- 2. Write a function which gives the sum of three integers
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z


-- 3. Using the constant pi and the power operator ^, write a function which gives
-- the area of a circle given its radius
areaOfCircle :: Float -> Float
areaOfCircle r = pi * (r^2)


-- 4. Using the definition of areaOfCircle, write a function that gives the colume
-- of a cylinder given length and cross sectional radius
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = h * (areaOfCircle r)


-- 5. Write a function that takes four floats representing the coordinates
-- x1, y1, x2, y2 of two point, and gives the distance between the points
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)


-- 6. Write a function which returns True if, and only if, all of its three parameters
-- are all different from one another
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c

-- 7. Using the 'mod' function, write a function that tests whether one integer is
-- divisible by another
divisibleBy :: Int -> Int -> Bool
a `divisibleBy` b = a `mod` b == 0


-- 8. Using the definition of divisibleBy, write a function which determines whether
-- its argument is an even number
isEven :: Int -> Bool
isEven x = x `divisibleBy` 2


-- 9. Write a function which gives the average of three integer values
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = ((fromIntegral x) + (fromIntegral y) + (fromIntegral z)) / 3


-- 10. Using a conditional expression (and not the built-in function 'abs'), write
-- a function that gives the absolute value of an integer (i.e gives a non-negative
-- value)
absolute :: Int -> Int
absolute x = if x < 0 then x * (-1) else x
