import Prelude hiding ((!!))
f = [True, True, False]
t = [True, True, True]

andA :: [Bool] -> Bool
andA [] = True
andA (b:bs) = b && andA bs

andB :: [Bool] -> Bool
andB [] = True
andB (b:bs) | b = andB bs
            | otherwise = False

andC :: [Bool] -> Bool
andC [] = False
andC (b:bs) = b && andC bs

andD :: [Bool] -> Bool
andD [] = False
andD (b:bs) = b || andD bs

andE :: [Bool] -> Bool
andE [] = True
andE (b:bs) | b == False = False
            | otherwise = andE bs

andF :: [Bool] -> Bool
andF [] = True
andF (b:bs) = b || andF bs

andG :: [Bool] -> Bool
andG [] = True
andG (b:bs) = andG bs && b

andH :: [Bool] -> Bool
andH [] = True
andH (b:bs) | b = b
            | otherwise = andH bs

mergeA :: Ord a => [a] -> [a] -> [a]
mergeA [] ys = ys
mergeA (x:xs) (y:ys) = if x <= y
                       then x : mergeA xs ys
                       else y : mergeA xs ys

mergeB :: Ord a => [a] -> [a] -> [a]
mergeB [] ys = ys
mergeB xs [] = xs
mergeB (x:xs) (y:ys) = if x <= y
                       then y : mergeB xs (y:ys)
                       else x : mergeB (x:xs) ys
mergeC :: Ord a => [a] -> [a] -> [a]
mergeC [] ys = ys
mergeC xs [] = xs
mergeC (x:xs) (y:ys) = if x <= y
                       then y : mergeC (x:xs) ys
                       else x : mergeC xs (y:ys)

mergeD :: Ord a => [a] -> [a] -> [a]
mergeD [] ys = ys
mergeD xs [] = xs
mergeD (x:xs) (y:ys) = if x <= y
                       then x : mergeD xs (y:ys)
                       else y : mergeD (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msortA :: Ord a => [a] -> [a]
msortA [] = []
msortA xs = mergeD (msortA zs) (msortA ys)
  where (ys, zs) = halve xs

msortB :: Ord a => [a] -> [a]
msortB [] = []
msortB [x] = [x]
msortB xs = mergeD (msortB ys) (msortB zs)
  where (ys, zs) = halve xs

msortC :: Ord a => [a] -> [a]
msortC [] = []
msortC [x] = [x]
msortC xs = msortC ys ++ msortC zs
  where (ys, zs) = halve xs

msortD :: Ord a => [a] -> [a]
msortD [] = []
msortD [x] = [x]
msortD xs = msortD (msortD ys ++ msortD zs)
  where (ys, zs) = halve xs

