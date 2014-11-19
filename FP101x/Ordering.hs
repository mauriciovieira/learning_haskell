isMonotone :: Ord a => [a] -> Bool
isMonotone [] = True
isMonotone xs = and [x <= y | (x, y) <- zip xs (tail xs)]

