module Lib where

preprocess :: String -> String
preprocess = filter (not . (`elem` ['!','.']))

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
    where
        cleanText = preprocess text