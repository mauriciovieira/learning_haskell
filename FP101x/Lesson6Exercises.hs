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

{-
anyC :: (a -> Bool) -> [a] -> Bool
anyC p xs = length (filter p xs) > 0

testAnyCTrue     = anyC (\d -> d >= 0 && d < 10) [1..10]
testAnyCTrueInf  = anyC (\d -> d >= 0 && d < 10) [1..]
testAnyCFalse    = anyC (\d -> d >= 0 && d < 10) [11..20]

anyD :: (a -> Bool) -> [a] -> Bool

testAnyDTrue     = anyD (\d -> d >= 0 && d < 10) [1..10]
testAnyDTrueInf  = anyD (\d -> d >= 0 && d < 10) [1..]
testAnyDFalse    = anyD (\d -> d >= 0 && d < 10) [11..20]

anyE :: (a -> Bool) -> [a] -> Bool

testAnyETrue     = anyE (\d -> d >= 0 && d < 10) [1..10]
testAnyETrueInf  = anyE (\d -> d >= 0 && d < 10) [1..]
testAnyEFalse    = anyE (\d -> d >= 0 && d < 10) [11..20]

anyF :: (a -> Bool) -> [a] -> Bool

testAnyFTrue     = anyF (\d -> d >= 0 && d < 10) [1..10]
testAnyFTrueInf  = anyF (\d -> d >= 0 && d < 10) [1..]
testAnyFFalse    = anyF (\d -> d >= 0 && d < 10) [11..20]

anyG :: (a -> Bool) -> [a] -> Bool

testAnyGTrue     = anyG (\d -> d >= 0 && d < 10) [1..10]
testAnyGTrueInf  = anyG (\d -> d >= 0 && d < 10) [1..]
testAnyGFalse    = anyG (\d -> d >= 0 && d < 10) [11..20]

anyH :: (a -> Bool) -> [a] -> Bool

testAnyHTrue     = anyH (\d -> d >= 0 && d < 10) [1..10]
testAnyHTrueInf  = anyH (\d -> d >= 0 && d < 10) [1..]
testAnyHFalse    = anyH (\d -> d >= 0 && d < 10) [11..20]
-}

