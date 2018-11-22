


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' (x:xs) 
        | (p x) = x:takeWhile' p xs
        | otherwise = []
