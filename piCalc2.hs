terms :: (Fractional a, Ord a, Enum a) => [a]
terms = zipWith (*) signs positiveTerms
    where positiveTerms = (map (\x -> 4.0 / x) [1,3..])
          signs = [(-1) ^ n | n <- [0,1..]]

piCalc :: (Fractional a, Ord a, Enum a) => a -> (a, Int)
piCalc tolerance = ((foldl1 (+) neededTerms), length neededTerms)
    where neededTerms = (takeWhile ((>= tolerance).abs) terms)
