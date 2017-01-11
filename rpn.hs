solveRPN :: (Num a, Read a) => String -> a
-- solveRPN expression = head (foldl foldingFunction [] (words expression))
solveRPN = head . foldl foldingFunction [] . words
    where  foldingFunction (x:y:ys) "*" = (y * x):ys
           foldingFunction (x:y:ys) "+" = (y + x):ys
           foldingFunction (x:y:ys) "-" = (y + x):ys
           foldingFunction xs numberString = read numberString:xs

-- doesn't silently discard extra stack elements after the RPN expression has been evaluated
solveRPN' :: (Num a, Read a, Show a) => String -> a
solveRPN' = headOnly . resultList
    where resultList :: (Num a, Read a) => String -> [a]
          resultList = foldl foldingFunction [] . words
              where  foldingFunction (x:y:ys) "*" = (y * x):ys
                     foldingFunction (x:y:ys) "+" = (y + x):ys
                     foldingFunction (x:y:ys) "-" = (y + x):ys
                     foldingFunction xs numberString = read numberString:xs
          headOnly (x:[]) = x
          headOnly xs = error ("solveRPN: more than one item left on the stack: " ++ show xs)
