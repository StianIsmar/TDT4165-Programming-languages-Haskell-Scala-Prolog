listC :: (RealFloat a) =>  [a] -> String
listC [] = "Empty"
listC xs = case xs of (x:[]) -> "A singleton list"
                      (y:z:[]) -> "A list with two elements"
                      (y:z:_)-> "A list of many elements"



ageCheck :: (Integral a) => a -> String
ageCheck y
    | calculated < 10 = "Personen er mindre enn 10"
    | calculated < 12 = "Personen er mindre enn 12"
    | calculated < 15 = "Personen er mindre enn 15"
    | otherwise = "Emot"
    where calculated = y
