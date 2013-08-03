compress :: Eq a => [a] -> [a]
compress = foldr consIfNe []
    where consIfNe x [] = [x]
          consIfNe x ls@(y:ys)
                | x == y        = ls
                | otherwise     = x:ls

compress2 :: Eq a => [a] -> [a]
compress2 (x:(xs@(y:ys)))
    | x == y        = compress2 xs   
    | otherwise     = x : compress2 xs
compress2 xs = xs

compress3 :: Eq a => [a] -> [a]
compress3 xs = (map fst $ filter (not . isSame) (zip xs (tail xs))) ++ [last xs]
    where isSame (x, y) = x == y
