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

         -- If the current one is smaller (or ==) than the
            -- next one in the list-> set the next one as
            -- current and call it recursively! easy
listMaximum :: (Ord a) => [a] -> Maybe a
listMaximum [] = Nothing
listMaximum (x:xs) = Just $ listMaximum' x xs
        where 
            listMaximum' current [] = current
            listMaximum' current (x:xs)
                | current <= x = listMaximum' x xs
                | otherwise = listMaximum' current xs


listMinimum :: (Ord a) => [a] -> Maybe a
listMinimum [] = Nothing
listMinimum (x:xs) = Just $ listMin' x xs
    where 
        listMin' current [] = current
        listMin' current (x:xs)
            | current >= x = listMin' x xs
            | otherwise = listMin' current xs
               

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

-- predicate (>2) på listen [3,1,4]
all :: Foldable t => (a -> Bool) -> t a -> Bool 
all p xs = foldr (\x acc -> ifOneFalse x acc) True xs
            where
                ifOneFalse x acc
                    | p x == False = False
                    | otherwise = acc
           

-- TASK 4
-- Num Complex
 
data Complex = Complex Double Double deriving (Eq) 
 
instance Show Complex where 
    show (Complex r i) 
        | i >= 0 = show r ++ "+" ++ show i ++ "i" 
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i" 

instance Num Complex where 
    (+) (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1+i2) 
    (*) (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (i1*r2 + i2*r1) --usikker!
    --r1r2 + r1i2 + i1r2 +i1i2 
    abs (Complex r1 i1) = Complex (sqrt $ r1^2 + i1^2) 0 
    signum (Complex r1 i1) = Complex (r1/l) (i1/l)
                                where l = sqrt (r1^2 + i1^2)
    fromInteger x = Complex (fromInteger x) 0
    negate (Complex r2 i2) = Complex (-r2) (-i2) 

-- TASK 5
-- Making your own type classes

type Position = (Double, Double)

class Pos a where
    pos :: a -> Position

data Campus = Kalvskinnet| Gløshaugen| Tyholt| Moholt| Dragvoll deriving (Show, Eq)

instance Pos Campus where
    pos Kalvskinnet = (63.429, 10.388)
    pos Gløshaugen  = (63.416, 10.403)
    pos Tyholt      = (63.423, 10.435)
    pos Moholt      = (63.413, 10.434)
    pos Dragvoll    = (63.409, 10.471)

--Type class Move that is a subclass of Pos:
class (Pos a) => Move a where
    move :: a -> Position -> a
    belongs :: a -> Position

-- Type Car with record syntax:
data Car = Car { 
                 brand :: String , 
                 regNr :: String,
                 isAt :: Position,
                 key :: Key,
                 parking :: Position
               } deriving (Show)
               --Should derive Eq, Pos, and Move:

instance Eq Car where
    (==) c1 c2 = regNr c1 == regNr c2

    -- Gir ut attributt_
instance Pos Car where
    pos c1 = isAt c1
    -- Oppdaterer attributt:
instance Move Car where
    move c1 newpos = c1 {isAt = newpos}  
    -- Gir ut attributt av typen Position:
    belongs c1 = parking c1

data Key = Key {
                    keyNr::Int,
                    located :: Position,
                    cabinet :: Position
                }, deriving (Show)

instance Eq Key where
    (==) k1 k2 = keyNr k1 == keyNr k2

instance Pos Key where
    pos k1 = located k1

instance Move Key where
move k1 newpos = k1 {located = newpos}
belongs k1 = cabinet k1

--Check if an object is where it belongs:
free :: Move a => a -> Bool
free t = belongs t == pos t

carAvailable :: Car -> Bool
carAvailable c1 = free t && (free (key c1))

distanceBetween :: Pos a => a -> a -> (Position or Int)
distanceBetween loc object = (abs $ loc1-obj1, abs $ loc1 - obj2)
                    where
                        (loc1,loc2) = pos loc
                        (obj,obh2) = pos object