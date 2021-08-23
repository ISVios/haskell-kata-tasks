module Disemvowel where

disemvowel :: String -> String
disemvowel = filter isNotVowels
  where
    isNotVowels 'A' = False
    isNotVowels 'a' = False
    isNotVowels 'E' = False
    isNotVowels 'e' = False
    isNotVowels 'I' = False
    isNotVowels 'i' = False
    isNotVowels 'O' = False
    isNotVowels 'o' = False
    isNotVowels 'U' = False
    isNotVowels 'u' = False
    --isNotVowels 'Y' = False
    --isNotVowels 'y' = False
    isNotVowels _   = True
