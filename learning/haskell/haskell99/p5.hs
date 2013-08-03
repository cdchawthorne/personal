myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' xs = (last xs) : (reverse $ init xs)

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

myReverse''' :: [a] -> [a]
myReverse''' = foldr ((flip (++)) . (:[])) []
