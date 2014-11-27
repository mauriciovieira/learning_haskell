twice :: (a -> a) -> a -> a
twice f x = f (f x)

map':: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

mapR :: (a -> b) -> [a] -> [b]
mapR f [] = []
mapR f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [ x | x <- xs, f x]

filterR :: (a -> Bool) -> [a] -> [a]
filterR f [] = []
filterR f (x:xs) | f x = x : filterR f xs
                 | otherwise = filterR f xs
