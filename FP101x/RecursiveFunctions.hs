import Prelude hiding ((++), (!!))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

drop' :: Int -> [a] ->[a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop (n-1) xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)
[] !! _ = error "Out of bounds"

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

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
