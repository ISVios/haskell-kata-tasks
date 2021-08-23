module TitleCase where
--
--
import Data.Char (toLower, toUpper)
--
--
titleCase :: String -> String -> String
titleCase minor title = unwords $ head tList : conv mList (tail tList) 
  where
    lowMode = map toLower 
    mList   = words . lowMode $ minor
    tList   = map (\(x:xs) -> (toUpper  x) : map (toLower) xs) . words $ title
    conv :: [String] -> [String] -> [String]
    conv _  []     = []
    conv ms (x:xs) | (lowMode x) `elem` ms = (lowMode x) : conv ms xs
                   | otherwise             = x : conv ms xs 
--
--
