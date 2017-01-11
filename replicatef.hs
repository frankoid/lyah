--replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' :: (Integral i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:(replicate' (n-1) x)
