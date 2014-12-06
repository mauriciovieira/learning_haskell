{-- Exercise 1
 --}
allA :: (a -> Bool) -> [a] -> Bool
allA p xs = and (map p xs)

testAllA = allA (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]

{-
allB :: (a -> Bool) -> [a] -> Bool
allB p xs = map p (and xs)

testAllB = allB (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]
-}

allC :: (a -> Bool) -> [a] -> Bool
allC p = and . map p

testAllC = allC (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]

allD :: (a -> Bool) -> [a] -> Bool
allD p = not . any (not . p)

testAllD = allD (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]

{-
allE :: (a -> Bool) -> [a] -> Bool
allE p = map p . and

testAllE = allE (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]
-}

allF :: (a -> Bool) -> [a] -> Bool
allF p xs = foldl (&&) True (map p xs)

testAllF = allF (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]

allG :: (a -> Bool) -> [a] -> Bool
allG p xs = foldr (&&) False (map p xs)

testAllG = allG (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]

allH :: (a -> Bool) -> [a] -> Bool
allH p = foldl (&&) True . map p

testAllH = allH (\d -> d >= 0 && d < 10) [1, 3, 5, 6, 7, 5]

{-- Exercise 2
 --}
{-
anyA :: (a -> Bool) -> [a] -> Bool
anyA p = map p . or

testAnyATrue     = anyA (\d -> d >= 0 && d < 10) [1..10]
testAnyATrueInf  = anyA (\d -> d >= 0 && d < 10) [1..]
testAnyAFalse    = anyA (\d -> d >= 0 && d < 10) [11..20]
-}

anyB :: (a -> Bool) -> [a] -> Bool
anyB p = or . map p

testAnyBTrue     = anyB (\d -> d >= 0 && d < 10) [1..10]
testAnyBTrueInf  = anyB (\d -> d >= 0 && d < 10) [1..]
testAnyBFalse    = anyB (\d -> d >= 0 && d < 10) [11..20]

anyC :: (a -> Bool) -> [a] -> Bool
anyC p xs = length (filter p xs) > 0

testAnyCTrue     = anyC (\d -> d >= 0 && d < 10) [1..10]
testAnyCTrueInf  = anyC (\d -> d >= 0 && d < 10) [1..] -- does not end
testAnyCFalse    = anyC (\d -> d >= 0 && d < 10) [11..20]

anyD :: (a -> Bool) -> [a] -> Bool
anyD p = not . null . dropWhile (not . p)

testAnyDTrue     = anyD (\d -> d >= 0 && d < 10) [1..10]
testAnyDTrueInf  = anyD (\d -> d >= 0 && d < 10) [1..]
testAnyDFalse    = anyD (\d -> d >= 0 && d < 10) [11..20]

anyE :: (a -> Bool) -> [a] -> Bool
anyE p = null . filter p

testAnyETrue     = anyE (\d -> d >= 0 && d < 10) [1..10]
testAnyETrueInf  = anyE (\d -> d >= 0 && d < 10) [1..]
testAnyEFalse    = anyE (\d -> d >= 0 && d < 10) [11..20]

anyF :: (a -> Bool) -> [a] -> Bool
anyF p xs = not (all (\ x -> not (p x)) xs)

testAnyFTrue     = anyF (\d -> d >= 0 && d < 10) [1..10]
testAnyFTrueInf  = anyF (\d -> d >= 0 && d < 10) [1..]
testAnyFFalse    = anyF (\d -> d >= 0 && d < 10) [11..20]

anyG :: (a -> Bool) -> [a] -> Bool
anyG p xs = foldr (\ x acc -> (p x) || acc) False xs

testAnyGTrue     = anyG (\d -> d >= 0 && d < 10) [1..10]
testAnyGTrueInf  = anyG (\d -> d >= 0 && d < 10) [1..]
testAnyGFalse    = anyG (\d -> d >= 0 && d < 10) [11..20]

anyH :: (a -> Bool) -> [a] -> Bool
anyH p xs = foldr (||) True (map p xs)

testAnyHTrue     = anyH (\d -> d >= 0 && d < 10) [1..10]
testAnyHTrueInf  = anyH (\d -> d >= 0 && d < 10) [1..]
testAnyHFalse    = anyH (\d -> d >= 0 && d < 10) [11..20]

{-- Exercise 3
 --}
oneToTen = [1..10]

testTakeWhileTrue     = takeWhile (\d -> d >= 0 && d < 10) [1..10]  == [1 ..  9]
testTakeWhileTrueInf  = takeWhile (\d -> d >= 2 && d < 20) [2..]    == [2 .. 19]
testTakeWhileFalse    = takeWhile (\d -> d >= 0 && d < 10) [11..20] /= [       ]

takeWhileA :: (a -> Bool) -> [a] -> [a]
takeWhileA _ [] = []
takeWhileA p (x:xs) | p x = x : takeWhileA p xs
                    | otherwise = takeWhileA p xs

testTakeWhileATrue    = takeWhileA (\d -> d >= 0 && d < 10) [1..10]  == [1 ..  9]
testTakeWhileATrueInf = takeWhileA (\d -> d >= 2 && d < 20) [2..]    == [2 .. 19]
testTakeWhileAFalse   = takeWhileA (\d -> d >= 0 && d < 10) [11..20] /= [       ]

takeWhileB :: (a -> Bool) -> [a] -> [a]
takeWhileB _ [] = []
takeWhileB p (x:xs) | p x = x : takeWhileB p xs
                    | otherwise = []

testTakeWhileBTrue    = takeWhileB (\d -> d >= 0 && d < 10) [1..10]  == [1 ..  9]
testTakeWhileBTrueInf = takeWhileB (\d -> d >= 2 && d < 20) [2..]    == [2 .. 19]
testTakeWhileBFalse   = takeWhileB (\d -> d >= 0 && d < 10) [11..20] /= [       ]

takeWhileC :: (a -> Bool) -> [a] -> [a]
takeWhileC _ [] = []
takeWhileC p (x:xs) | p x = takeWhileC p xs
                    | otherwise = []

testTakeWhileCTrue    = takeWhileC (\d -> d >= 0 && d < 10) [1..10]  == [1 ..  9]
testTakeWhileCTrueInf = takeWhileC (\d -> d >= 2 && d < 20) [2..]    == [2 .. 19]
testTakeWhileCFalse   = takeWhileC (\d -> d >= 0 && d < 10) [11..20] /= [       ]

takeWhileD :: (a -> Bool) -> [a] -> [a]
takeWhileD p = foldl (\ acc x -> if p x then x : acc else acc) []

testTakeWhileDTrue    = takeWhileD (\d -> d >= 0 && d < 10) [1..10]  == [1 ..  9]
testTakeWhileDTrueInf = takeWhileD (\d -> d >= 2 && d < 20) [2..]    == [2 .. 19]
testTakeWhileDFalse   = takeWhileD (\d -> d >= 0 && d < 10) [11..20] /= [       ]

{-- Exercise 5
 - https://www.haskell.org/haskellwiki/Fold
 --}
foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)

pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

result5R = "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+0)))))))))))))"
result5L = "(((((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)+11)+12)+13)"
result5T = "((((1+2)+(3+4))+((5+6)+(7+8)))+(((9+10)+(11+12))+13))"

testMapR = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13]) == result5R
testMapL = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13]) == result5L
testMapT = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13]) == result5T

mapA :: (a -> b) -> [a] -> [b]
mapA f = foldr (\ x xs -> xs ++ [f x]) []

testMapAR = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (mapA show [1..13]) == result5R
testMapAL = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (mapA show [1..13]) == result5L
testMapAT = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (mapA show [1..13]) == result5T

{-
mapB :: (a -> b) -> [a] -> [b]
mapB f = foldr (\ x xs -> f x ++ xs) []

testMapBR = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (mapB show [1..13]) == result5R
testMapBL = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (mapB show [1..13]) == result5L
testMapBT = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (mapB show [1..13]) == result5T
-}

mapC :: (a -> b) -> [a] -> [b]
mapC f = foldl (\ xs x -> f x : xs) []

testMapCR = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (mapC show [1..13]) == result5R
testMapCL = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (mapC show [1..13]) == result5L
testMapCT = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (mapC show [1..13]) == result5T

mapD :: (a -> b) -> [a] -> [b]
mapD f = foldl (\ xs x -> xs ++ [f x]) []

testMapDR = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (mapD show [1..13]) == result5R
testMapDL = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (mapD show [1..13]) == result5L
testMapDT = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (mapD show [1..13]) == result5T


{- Exercise 7
 -}
dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0

testDec2Int = dec2int [2,3,5,6,8] == 23568

{- Exercise 8 -}
-- sumsqreven = compose [sum, map (^2), filter even]
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
