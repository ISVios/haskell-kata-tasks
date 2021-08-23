module HighestAndLowest where
--
--
highAndLow :: String -> String
highAndLow input = (show . maximum $ numbers) ++ " " ++ (show . minimum $ numbers)
  where
    vals    = words input
    numbers = map (\x -> read x :: Int) vals
--
--
