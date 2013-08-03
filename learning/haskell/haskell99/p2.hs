myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (x::xs) = myButLast xs

myButLast1 :: [a] -> a
myButLast1 = head . tail . reverse
