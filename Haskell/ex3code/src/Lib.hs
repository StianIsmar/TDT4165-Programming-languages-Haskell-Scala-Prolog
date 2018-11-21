module Lib
    ( listSum
    , listProduct
    , listConcat
    , listMaximum
    , listMinimum
    , sum
    , concat
    , length
    , elem
    , safeMaximum
    , safeMinimum
    , any
    , all
    , foldr
    , Complex(..)
    ) where

import Prelude hiding (foldr, maximum, minimum, any, all, length
                      , concat, sum, product, elem, Foldable(..))

-- TASK 2
-- Bounded parametric polymorphism

-- Implement the following functions that reduce a list to a single
-- value (or Maybe a single value).

-- Maybe is imported from Prelude and is defined like this:
-- data Maybe a = Just a | Nothing

listSum :: (Num a) => [a] -> a
listSum [] = 0
listSum (x:xs) = x + listSum xs 

listProduct :: (Num a) => [a] -> a
listProduct [] = 1
listProduct (x:xs) = x * listProduct xs

listConcat :: [[a]] -> [a]
listConcat [] = []
listConcat (xs:xss)= xs ++ listConcat xss

listMaximum :: (Ord a) => [a] -> Maybe a
listMaximum [] = Nothing
listMaximum (x:xs) 
        | Just x > listMaximum xs = Just x
        | otherwise = listMaximum xs 



listMinimum :: (Ord a) => [a] -> Maybe a
listMinimum xs 
    | minlist2 xs == null = Nothing
    | otherwise = Just $ (minlist2 xs)
               

minlist2 :: (Ord a)=> [a]->a
minlist2 [] = null
minlist2 (x:xs)
        | x < minlist2 xs = x
        | otherwise = minlist2

-- TASK 3 Folds

-- TASK 3.1
-- Below our Foldable class is defined. Now define a list instance of
-- Foldable, and then define the Foldable versions of the functions
-- you defined previously (and some more).
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable [] where
  foldr _ acc [] = acc
  foldr f acc (x:xs) = x `f` foldr f acc xs   

--
-- USE FOLDR TO DEFINE THESE FUNCTIONS
--
sum :: (Num a, Foldable t) => t a -> a
sum xs= foldr (+) 0 xs

concat :: Foldable t => t [a] -> [a]
concat xs= foldr (++) [] xs

length :: Foldable t => t a -> Int
length xs = foldr (\_ acc -> acc + 1) 0 xs


elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem e xs = foldr (\x acc -> if checkIfContains e x then True else acc) False xs
    where checkIfContains check elem
            | check == elem = True
            | otherwise = False

--Når det er sammenligning av to blir det andre boller
safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum xs = foldr (\x acc -> checkElem x acc) Nothing xs
    where
        checkElem x Nothing = Just x
        checkElem x (Just y)
            | x > y = Just x
            | otherwise = Just y
   

safeMinimum :: (Foldable t, Ord a) => t a -> Maybe a
safeMinimum xs = foldr (\x acc -> min' x acc) Nothing xs
        where 
            min' x Nothing = Just x
            min' x (Just y)
                | x < y = Just x
                | otherwise = Just y

-- The functions "any" and "all" check if any or all elements of a
-- Foldable satisfy the given predicate.
--
-- USE FOLDR
--
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p xs = foldr (\x acc -> if p x then True else acc ) False xs

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p xs = foldr (\x acc -> ifOneFalse x) True xs
            where
                ifOneFalse x
                    | p x == False = False

-- TASK 4
-- Num Complex
 
data Complex = Complex Double Double deriving (Eq) 
 
instance Show Complex where 
    show (Complex r i) 
        | i >= 0 = show r ++ "+" ++ show i ++ "i" 
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i" 

instance Num Complex where 
    (+) = undefined
    (*) = undefined
    abs = undefined 
    signum = undefined
    fromInteger = undefined 
    negate = undefined 

-- TASK 5
-- Making your own type classes

type Position = (Double, Double)

class Pos a where
    pos :: a -> Position

data Campus = Kalvskinnet
            | Gløshaugen
            | Tyholt
            | Moholt
            | Dragvoll
            deriving (Show, Eq)

instance Pos Campus where
    pos Kalvskinnet = (63.429, 10.388)
    pos Gløshaugen  = (63.416, 10.403)
    pos Tyholt      = (63.423, 10.435)
    pos Moholt      = (63.413, 10.434)
    pos Dragvoll    = (63.409, 10.471)

--class (Pos a) => Move a where
