myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myLast1 = foldl1 $ const id
-- const id == flip const

myLast2 = foldr1 $ flip const
myLast3 = head . reverse
