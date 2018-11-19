module Lib
    ( f0
    , f1
    , f2
    , take
    , map
    , iterate
    , filterPos
    , filterPosMany
    , flip3
    , Maybe(..)
    , safeHeadList
    , safeHead
    ) where

import Prelude hiding (map, take, iterate, sqrt, Maybe)

-- TASK 1
-- Parametric polymorphism

-- Below are three type signatures. Can you implement them? We
-- say a function or implementation /inhabits/ it's type
-- (signature). How many other inhabitants do these types
-- have? What if we fixed a = Int, does that change your
-- previous answer?
f0 :: a -> a
f0 x = x

f1 :: a -> b -> a
f1 x _ = x

f2 :: a -> b -> b
f2  _ y = y

-- Rewrite the function "takeInt" from exercice 1 as "take" so
-- that it accepts a list of any type. If you used the
-- built-in function "take" on the last assignment, write your
-- own implementation this time. Be sure to include a type
-- signature. (Hint: If you already wrote takeInt, you won't
-- have to change much.)

-- take take a number n and a list, and return the n first elements
take = takeInt

takeInt ::Int -> [a] -> [a]
takeInt _ [] = []
takeInt n (x:xs)
    | n <= 0 = []
    | n > length xs+1 = (x:xs)
    | otherwise = (x:takeInt (n-1) xs)

-- The function head :: [a] -> a which returns the first
-- element of a list, is /partial/, meaning it will crash for
-- some inputs. (Which?) One solution could be to make a
-- /total/ function "safeHeadList :: [a] -> [a]" which either
-- gives the head, or nothing. Can you implement it using take?
safeHeadList :: [a] -> [a]
safeHeadList [] = []
safeHeadList xs = takeInt 1 xs 

-- The output of safeHeadList is either empty or a singleton,
-- and thus using a list as output-type is a bit misleading. A
-- better choice is Maybe (sometimes called Optional):
data Maybe a = Some a | None deriving (Eq, Show)

-- Implement 'safeHead', representing failure using None.
safeHead :: [a] -> Maybe a
safeHead [] = None
safeHead (x:xs) = Some x 

-- TASK 2
-- Higher order functions

map :: (a -> b) -> [a] -> [b]
-- 1) Med list comprehensions:
    --map f xs = [f x | x <- xs]
-- 2) Med rekursjon (higher order):
    --map _ [] = []
    --map f (x:xs) = f x : map f xs 
-- 3) Med foldr (higher order):
map f xs = foldr (\x acc -> f x : acc) [] xs
    --TRIKS: Se hva den skal returnere!

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)  

-- TASK 3
-- Currying and partial application

-- complete the function filterPos
-- that takes a list and returns 
-- a filtered list containing only positive
-- integers (including zero)
-- use partial application to achieve this
filterPos :: [Int] -> [Int]
filterPos [] = []
filterPos (x:xs)
        | x >= 0 = x : filterPos xs
        | otherwise = filterPos xs

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers (including zero)
-- hint: use filterPos and map
filterPosMany :: [[Int]] -> [[Int]]
-- 1) With foldrfilterPosMany xss = foldr (\x acc-> filterPos x:acc) [[]] xss 
-- 2) With list comprehension:  -- filterPosMany xss = [filterPos x | x<-xss]
-- 3 ) With map:
filterPosMany xss = map filterPos xss

-- flip3 (\x y z -> x : y : [z]) ’c’ ’b’ ’a’
flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x

-- TASK 4
-- Infinite lists

approxSquare :: (Floating a ) => a -> a -> a
approxSquare x guess = guess - ((guess^2 - x)/(2 * guess))


-- Own: Function that return an infinite list of approximations:








isPerfSq :: Double -> Bool
isPerfSq = undefined

--uncomment when isPerfSqr is defined
--accuracy :: Int -> Bool
--accuracy x = take x generated == take x [x^2 | x <- [1..]]
--                where
--             zpd       = zip [1..] (map isPerfSq [1..])
--             f (x,y)   = y == True
--             generated = fst . unzip $ filter f zpd
