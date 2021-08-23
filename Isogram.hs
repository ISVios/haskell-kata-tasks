module Isogram where
--
import Data.Char (toLower, toUpper)
--
--isIsogram :: String -> Bool
isIsogram []     = True
isIsogram (x:xs) = (all id $ map (`notElem` [toLower x, toUpper x]) xs) && isIsogram xs
--
--
