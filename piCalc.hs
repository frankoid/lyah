{- Lets calculate pi.
 - The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
 - Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
 - We can create a function, where given a certain tolerance, we can recursively calculate
 - Pi to within that tolerance.
 - Lets create two functions, piCalc, and piCalc', the latter we will recursively call
 - until our pi calculation is within the tolerance
 - The piCalc function is defined as:
 - piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
 - Given a tolerance, say, 0.001, it will return a tuple.
 - fst is pi to an accuracy of the tolerance, 0.001 in this case
 - snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
 - Example: piCalc 0.001 = (3.1420924036835256,2000)
 - The piCalc' function is defined as
 - piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
 - Lots of parameters!
 - The first parameter is the current denominator from the Leibniz formula
 - The next is our calculation of pi from our previous attempt
 - The next is the tolerance
 - The final parameter is the number of times this function has been called (ie, we add one every time we recurse
 - Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)
 -
 - Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
 - But, of course the output of piCalc should remain as (pi, count)
 -
 - You may find the stepReverseSign function handy
 -}

piCalc :: (Fractional a, Ord a, Integral b) => a -> (a, b)
--piCalc :: (Fractional a, Ord a) => a -> (a, a)
piCalc tolerance = piCalc' 0 0 tolerance

piCalc' :: (Fractional a, Ord a, Integral b) => b -> a -> a -> (a, b)
--piCalc' :: (Fractional a, Ord a) => a -> a -> a -> (a, a)
piCalc' n piEstimate tolerance
    | abs(diff) < tolerance = (piEstimate, n)
    | otherwise = piCalc' (n + 1) (piEstimate + diff) tolerance
    where diff = (fromIntegral (4 * ((-1) ^ n))) / (fromIntegral ((2 * n) + 1))
