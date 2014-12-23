fibs :: [Int]
fibs = 0 : 1 : [x+y | (x,y) <- zip fibs (tail fibs)]

largefibA :: Int
largefibA = head (dropWhile (<= 1000) fibs)

largefibB :: Int
largefibB = last (take 19 fibs)

largefibC :: Int
largefibC = head $ filter (> 1000) fibs

largefibD :: Int
largefibD = head (drop 1000 fibs)

largefib :: Int
largefib = largefibA
