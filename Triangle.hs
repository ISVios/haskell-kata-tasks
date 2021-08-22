module Triangle where
--
-- task from www.codewars.com/kata/is-this-a-triangle
import Data.List (sort)
--
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c | a <= 0 || b <= 0 || c <= 0 = False
                 | a > b  || a > c  || b > c  = isTriangle a' b' c'
                 | otherwise                  = y > 0
  where
    [a',b',c'] = sort [a, b, c]
    y          = -x^2 + b^2
    x          = (a^2 + b^2 - c^2) `div` (2*a)

--
--
