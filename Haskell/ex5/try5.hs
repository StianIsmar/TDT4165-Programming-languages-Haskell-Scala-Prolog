--partial function

f :: [a] -> Int -> a 
f [] _ = 0
f (x:xs) n
        | n == 0 = x
        | otherwise = f xs
