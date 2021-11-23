{-# LANGUAGE OverloadedStrings #-}
module Lib (isPalindrome) where

import qualified Data.Text as T
import Data.Char (toLower,isSpace,isPunctuation)

stripWhiteSpace :: T.Text -> T.Text 
stripWhiteSpace = T.filter (not . isSpace)

stripPunctutation :: T.Text -> T.Text 
stripPunctutation = T.filter (not . isPunctuation)

preprocess :: T.Text -> T.Text 
preprocess = stripWhiteSpace . stripPunctutation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where
        cleanText = preprocess text