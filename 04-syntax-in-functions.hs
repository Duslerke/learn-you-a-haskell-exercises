-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit x
    | x == 0    = "zero"
    | x == 1    = "one"
    | x == 2    = "two"
    | x == 3    = "three"
    | x == 4    = "four"
    | x == 5    = "five"
    | x == 6    = "six"
    | x == 7    = "seven"
    | x == 8    = "eight"
    | x == 9    = "nine"
    | otherwise = "unknow"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (_, 0) = undefined
divTuple (x, y) = x / y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:xs) = True
threeZeroList _ = False
