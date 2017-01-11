max'' [] = error "bah"
max'' (x:xs)
      | x == (7 :: Int) = 1000 :: Int
      | otherwise = max'' xs
