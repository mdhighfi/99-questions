myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> Maybe a
myButLast (x:[]) = Nothing
myButLast (x:y:[]) = Just x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt xs idx = xs !! (idx - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = myReverse list == list
