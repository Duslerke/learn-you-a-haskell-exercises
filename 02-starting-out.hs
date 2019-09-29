{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK :: Int -> [a] -> a
findK k = (!! k)

-- Determine if list l is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
-- duplicate xs = concat [sublist  | el <- xs, let sublist = replicate 2 el]
duplicate :: [a] -> [a]
duplicate = concat . map (replicate 2)

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}

-- if you had to do this without recursion, then, maybe..
ziplike :: [a] -> [b] -> [(a,b)]
ziplike xs ys = [(an, bn) | n <- [0..shorterLength -1] , let (an, bn) = (xs !! n, ys !! n)]
    where shorterLength = min (length xs) (length ys)

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2]) <-- example does not include index of 3 in the first part!
-- going by the output, the function would look like the following, otherwise it's all the same, but where k+1 is used in a where part, it would be k
splitAtIndex :: Int -> [a] -> ([a], [a])
splitAtIndex k xs
    | k < 0 = error "can't have negative index!"
    | otherwise = (firstPart, secondPart)
    where firstPart = take (k+1) xs
          secondPart = [xs !! n | n <- [k+1..length xs -1]]

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: Int -> [a] -> [a]
dropK k xs
    | k < 0 = error "can't be negative index!"
    | otherwise = [xs !! n | n <- [0..length xs -1], n /= k]

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: Int -> Int -> [a] -> [a]
slice i k xs
    | i < 0 || k < 0    = error "can't be negative index!"
    | i > k             = error "end index should be higher than start index!"
    | otherwise         = [xs !! n | n <- [i..k-1], k-1 < length xs] -- I wonder if there should be a validation for when index is outside list length

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: a -> Int -> [a] -> [a] -- could do it with ++, but it's ineficient
insertElem x k [] = [x]
insertElem x k xs
    | k < 0             = error "can't be negative index!"
    | k >= length xs    = xs ++ [x] -- maybe should just throw an error, not sure..
    | otherwise         = [ build n | n <- [0..length xs]]
    where build n
            | n < k         = xs !! n
            | n > k         = xs !! (n-1)
            | otherwise     = x

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: Int -> [a] -> [a] -- mod works in an interesting ways when negative value provided, still consistent here though
rotate n xs = [xs !! i | i <- [0..listEnd], i >= rot] ++ [xs !! i | i <- [0..listEnd], i < rot] -- could rewrite without ++, but whatever.. :P
    where rot = mod n $ length xs
          listEnd = length xs -1
