{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Integer -> Integer -> Integer
sumOfSquares x y = x * x + y * y

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Integer -> Integer
lastDigit number = go number
    where
        go :: Integer -> Integer
        go number =
            if number < 0
            then mod (-number) 10
            else if number < 10 
                 then number
                 else mod number 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}
minmax :: Integer -> Integer -> Integer -> Integer 
minmax x y z = 
    let max = maximum([x, y, z])
        min = minimum([x, y, z])
    in max - min

{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}
subString :: Integer -> Integer -> String -> String
subString :: Int -> Int -> String -> String
subString start end str
    | start > end = ""
    | end < 0     = ""
    | otherwise   = 
                    if start < 0
                    then drop 0 (reverse (drop ((length str) - end - 1) (reverse str)))
                    else drop start (reverse (drop ((length str) - end - 1) (reverse str)))

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: String -> Integer
strSum s = sum (map read (words s))

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greater than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}
lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater n list = show(n) ++ " is greater than " ++ show(lesserThanN 0 list) ++ " elements and lower than " ++ show(greaterThanN 0 list) ++ " elements"
    where
        greaterThanN :: Int -> [Int] -> Int
        greaterThanN greater l =
            if null l
            then greater
            else if head l > n
                 then greaterThanN (greater + 1) (tail l)
                 else greaterThanN greater       (tail l)

        lesserThanN :: Int -> [Int] -> Int
        lesserThanN lesser l =
            if null l
            then lesser
            else if head l < n
                 then lesserThanN (lesser + 1) (tail l)
                 else lesserThanN lesser       (tail l)