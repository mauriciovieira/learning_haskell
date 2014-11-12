safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB [] = []
safetailB (_:xs) = xs

safetailC :: [a] -> [a]
safetailC (_:xs) | null xs = []
                 | otherwise = tail xs

safetailD :: [a] -> [a]
safetailD xs | null xs = []
             | otherwise = tail xs

{-
safetailE :: [a] -> [a]
safetailE xs = tail xs
safetailE [] = []
-}

safetailF :: [a] -> [a]
safetailF [] = []
safetailF xs = tail xs 

safetailG :: [a] -> [a]
safetailG [x] = [x]
safetailG (_:xs) = xs

safetailH :: [a] -> [a]
safetailH = \ xs -> case xs of
                         [] -> []
                         (_:xs) -> xs

e3 x = x * 2
