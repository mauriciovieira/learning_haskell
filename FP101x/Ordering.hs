isMonotone :: Ord a => [a] -> Bool
isMonotone [] = True
isMonotone xs = and [x <= y | (x, y) <- zip xs (tail xs)]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [ a | a <-xs, a <= x ]
                 larger = [ b | b <- xs, b > x ]
