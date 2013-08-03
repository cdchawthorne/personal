factTransform :: (Int -> Int) -> Int -> Int
factTransform _ 0 = 1
factTransform possFact n = n * (possFact (n - 1))

y :: (a -> a) -> a
y f = f (y f)

y2Transform :: ((a -> a) -> a) -> (a -> a) -> a
y2Transform f g = g (f g)
