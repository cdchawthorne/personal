elementAt :: [a] -> Int -> a
elementAt xs = (!!) xs . (+ (-1))

elementAt1 :: [a] -> Int -> a
