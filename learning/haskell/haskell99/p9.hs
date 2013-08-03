pack :: Eq a => [a] -> [[a]]
pack = reverse . flip packHelp []

packHelp :: Eq a => [a] -> [[a]] -> [[a]]
packHelp [] acc = acc
packHelp (x:xs) [] = packHelp xs [[x]]
packHelp (x:xs) ((y:ys):zs)
    | x == y        = packHelp xs ((x:y:ys):zs)
    | otherwise     = packHelp xs ([x]:(y:ys):zs)

pack2 :: Eq a => [a] -> [[a]]
pack2 = foldr acc []
    where acc x ((y:ys):zs)
              | y == x      = (x:y:ys):zs
              | otherwise   = [x]:(y:ys):zs
          acc x [] = [[x]]

pack3 :: Eq a => [a] -> [[a]]
pack3 [] = []
pack3 lst@(x:xs) = 
    let splitList = span (== x) lst
    in [fst splitList] ++ (pack3 $ snd splitList)
