data Surfer = Surfer { firstName :: String,
                lastName :: String,
                boardSize :: Float
               } deriving (Show)


myFunc ::(Eq a,Num a) => a -> Bool
myFunc x
    | x == 2 = True
    | otherwise = False