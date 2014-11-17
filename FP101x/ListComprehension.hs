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


riffleA :: [a] -> [a] -> [a]
riffleA xs ys = concat [ [x,y] | x <- xs, y <- ys ]

riffleB :: [a] -> [a] -> [a]
riffleB xs ys = concat [ [x,y] | (x, y) <- xs `zip` ys ]

{- parse error on input ‘|’
riffleC :: [a] -> [a] -> [a]
riffleC xs ys = [ x, y | (x, y)  <- xs `zip` ys ]
-}

{-Couldn't match expected type ‘a’ with actual type ‘[a]’
riffleD :: [a] -> [a] -> [a]
riffleD xs ys = [ x : [y] | x <- xs, y <- ys ]
-}

pythsA :: Int -> [(Int, Int, Int)]
pythsA n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 + y^2 == z^2]

pythsB :: Int -> [(Int, Int, Int)]
pythsB n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]

pythsC :: Int -> [(Int, Int, Int)]
pythsC n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

pythsD :: Int -> [(Int, Int, Int)]
pythsD n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y]]

