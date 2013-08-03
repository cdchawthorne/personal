dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

dupli2 :: [a] -> [a]
dupli2 = foldr (\x -> (x:) . (x:)) []

dupli3 :: [a] -> [a]
dupli3 xs = [ x | x <- xs, y <- [0,1]]

dupli4 :: [a] -> [a]
dupli4 = concatMap (replicate 2)
