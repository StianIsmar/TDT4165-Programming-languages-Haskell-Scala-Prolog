--[1,2,3,4] til [4,3,2,1]
import Prelude hiding (map, filter)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs)++[x]


compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100

divide' :: (Floating a) => a -> a
divide' = (/10)

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

add' :: (Floating a)=> a -> a
add' = (/2)

filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs


largestDivisible :: (Integral a) => a  
largestDivisible = head (filter checkIfTrue [100000,99999..])  


checkIfTrue ::(Integral a)=> a -> Bool
checkIfTrue x
    |  (x `mod` 3829) == 0 = True
    | otherwise = False


getLastElem :: (Num a, Eq a) =>[a] -> [a]
getLastElem xs = scanr (\x acc -> if acc == 0 then x else acc) 0 xs