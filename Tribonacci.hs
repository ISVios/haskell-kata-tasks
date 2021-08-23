module Tribonacci where
--
--
tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci abc n = take n $ [e | (e, _, _) <- (iterate (\(x, y, z) -> (y,z,x+y+z)) abc)]
--
--
