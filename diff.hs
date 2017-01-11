--diff :: (Fractional a, Ord a, Integral b) => b -> a
--diff n = (4 * ((-1) ^^ n)) / ((2 * n) + 1)
diff :: (Fractional a, Integral b) => b -> a
diff n = (fromIntegral 4) / (fromIntegral n)
         
