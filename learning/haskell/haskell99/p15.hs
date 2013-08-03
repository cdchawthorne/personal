repli :: [a] -> Int -> [a]
repli = flip (concatMap . replicate)
