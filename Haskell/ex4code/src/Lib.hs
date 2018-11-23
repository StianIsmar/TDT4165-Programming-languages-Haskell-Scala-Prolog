module Lib
    ( Token(..)
    , Op(..)
    , takeWhile
    , dropWhile
    , break
    , splitOn
    , lex
    , tokenize
    , interpret
    , shunt
    ) where

import Prelude hiding (lex, dropWhile, takeWhile, break)
import Data.Char (isDigit)


takeWhile,dropWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) 
    | p x = x:takeWhile p xs
    | otherwise = []

dropWhile _ [] = []
dropWhile p (x:xs)
        | p x = dropWhile p xs
        | otherwise = x:xs

-- (\x-> x>5) [2,3,1,16,3,6]
break :: (a -> Bool) -> [a] -> ([a],[a])
break _ [] = ([],[])
break p xs = (takeWhile (not . p) xs, dropWhile (not . p ) xs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn i xs = if null x then (splitOn i y) else x:(splitOn i y)
        where (x,y) = break (\elem -> elem == i) $ (dropWhile (==i) xs)

data Token = TokOp Op | TokInt Int | TokErr deriving (Eq, Show)

data Op = Plus| Minus| Div| Mult | Hash | Dup deriving (Show, Eq)

-- Gjør om streng til liste med ett-elements-strenger:
lex :: String -> [String]
lex str = splitOn ' ' str

-- Tar inn strenger med ett element i en liste og gir ut tilhørende token:
tokenize :: [String] -> [Token]
tokenize [] = []
tokenize (x:xs) = (tokelem x) : tokenize xs

tokelem :: String -> Token
tokelem s
        | s == "-" = TokOp Minus
        | s == "+" = TokOp Plus
        | s == "*" = TokOp Mult
        | s == "/" = TokOp Div
        | s == "#" = TokOp Hash
        | s == "--" = TokOp Dup
        | isDigit (s !! 0) = TokInt $ (read s :: Int)
        | otherwise = TokErr
-- Tar inn [TokInt 3, TokInt 10, TokInt 9, 
--     TokOp Mult, TokOp Minus, TokInt 3, TokOp Plus]
-- Postfix
interpret :: [Token] -> [Token]
interpret xs = foldl (\acc x -> handleToken acc x ) [] xs
           

handleToken :: [Token] -> Token -> [Token]
handleToken ((TokInt x):(TokInt y):acc) (TokOp Minus) = [TokInt (y-x)]
handleToken ((TokInt x):(TokInt y):acc) (TokOp Plus) = [TokInt (y+x)]
handleToken ((TokInt x):(TokInt y):acc) (TokOp Mult) = [TokInt (y*x)]
handleToken ((TokInt x):(TokInt y):acc) (TokOp Div) = [TokInt (y `div` x)]
handleToken acc (TokInt x) = (TokInt x) : acc
handleToken (x:acc) (TokOp Hash) = x:x:acc
handleToken ((TokInt x):acc) (TokOp Dup) = (TokInt x):(TokInt (negate x)):acc
handleToken _ (TokErr) = [TokErr]

opLeq :: Token -> Token -> Bool
opLeq = undefined

shunt :: [Token] -> [Token]
shunt = undefined

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal = undefined
