{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , Status(..)
    , amountOf
    , fib
    , ending
    , takeInt
    , fizzbuzz
    , printFizz
    , listOfEven
    , zipped
    , cartesian
    ) where

import Control.Monad (mapM_)
import Prelude hiding (take)


-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add n m = n + m 

-- complete the function "isVowel" which
-- takes a character and returns True
-- if it's a vowel (English language), False otherwise
-- hint: a string is a list
-- hint2: use `elem` from Prelude
isVowel :: Char -> Bool
isVowel chr = chr `elem` "AEIOUaeiou"


data Status = One | Two | Three | None deriving (Show, Eq)

-- complete the function "amountOf" which takes
-- a name and a list of subjects
-- it then pattern matches on the amount of subjects in the list
-- and returns a tuple of a Status (look above, describes the amount of subjects in the list)
-- and the name
amountOf :: String -> [a] -> (Status, String)
amountOf name [] = (None,name)
amountOf name (x:[]) = (One, name)
amountOf name (x:y:[]) = (Two, name)
amountOf name (x:y:z:[]) = (Three, name)
 

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
-- do not optimize it
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

-- TASK 3
-- Working with lists

-- complete the function "ing" that takes a list of
-- strings and returns them with the ending -ing
-- if the string is empty, remove it from the list
    -- ["running", "swimming"]
ending :: [String] -> [String]
ending xs = [x ++ "ing" | x <- xs, length x > 0]

-- complete the function "takeInt" that
-- an integer n and a list of integers and
-- returns the first n elements of 
-- the list
-- [1,2,34,8,1,2]
takeInt :: Int -> [Int] -> [Int]
takeInt _ [] = []
takeInt n (x:xs)
    | n <= 0 = []
    | n > length xs+1 = (x:xs)
    | otherwise = (x:takeInt (n-1) xs)
    

-- implement "fizzbuzz" as described in exercise 1
fizzbuzz :: [String]
fizzbuzz = [checkElem x | x <- myList, length myList > 0]
    where myList = createList

checkElem :: Int -> String
checkElem x
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
    | x `mod` 3 == 0 = "Fizz"
    | x `mod` 5 == 0 = "Buzz"
    | otherwise = show x

createList :: [Int]
createList = [1..100]

printFizz :: IO ()
printFizz = mapM_ putStrLn fizzbuzz

-- TASK 4
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
-- use a list comprehension
listOfEven :: [Integer]
listOfEven = undefined

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
-- hint: parallel list comprehension
zipped :: [(Int, Char)]
zipped = undefined

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
-- use a list comprehension
cartesian :: [(Int, Int)]
cartesian = undefined
