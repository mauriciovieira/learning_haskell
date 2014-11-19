factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concatR :: [[a]] -> [a]
concatR [] = []
concatR (x:xs) = concat' xs ++ x

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = replicate' (n-1) x ++ [x]
