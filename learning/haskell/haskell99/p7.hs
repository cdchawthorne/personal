data NestedList a = List [NestedList a]
                  | Elem a
                  deriving (Show)

flatten :: NestedList a -> [a]
flatten (List xs) = concat $ map flatten xs
flatten (Elem x) = [x]

nestConcat :: [NestedList a] -> NestedList a
nestConcat (x:xs) = List ((toList x) ++ (toList $ nestConcat xs))
        where toList (List xs) = xs
              toList (Elem x) = [Elem x]
nestConcat [] = List []

nestMap :: (a -> b) -> NestedList a -> NestedList b
nestMap f (Elem x) = Elem $ f x
nestMap f (List xs) = List $ map (nestMap f) xs

instance Monad NestedList where
    (>>=) (Elem x) = ($ x)
    (>>=) (List xs) = nestConcat . (flip map) xs . flip (>>=)
    -- lst >>= f = nestConcat $ nestMap f lst

    (Elem x) >> lst = lst
    (List xs) >> lst = nestConcat $ map (>> lst) xs

    return = Elem

    fail str = List []

flatten2 :: NestedList a -> NestedList a
flatten2 = (>>= Elem)
