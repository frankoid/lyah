-- (a) a conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- (b) guard equations
safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs

-- (c) pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
--safetail'' xs = tail xs
safetail'' (x:xs) = xs
