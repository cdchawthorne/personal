myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Integer
myLength' = sum . (map $ const 1)

myLength'' :: [a] -> Integer
myLength'' = foldl (\x _ -> x+1) 0

myLength''' :: [a] -> Integer
myLength''' = fst . last . zip [1..]
