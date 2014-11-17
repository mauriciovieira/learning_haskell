pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs =
   and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs =
   [i | (x', i) <- zip xs [0..n], x == x']
   where n = length xs - 1

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs =
   [i | (x', i) <- zip xs [0..], x == x']

{-
lowers :: String -> Int
lowers xs =
   length [x | x <- xs, isLower x]
-}

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [ d | d <- [1..x], x `divides` d]
