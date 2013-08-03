pack :: Eq a => [a] -> [[a]]
pack [] = []
pack lst@(x:xs) = 
    let splitList = span (== x) lst
    in [fst splitList] ++ (pack $ snd splitList)

--encode :: Eq a => [a] -> [(Int, a)]
encode = map (\lst -> (length lst, head lst)) . pack
