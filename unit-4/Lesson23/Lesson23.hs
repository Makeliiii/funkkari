{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import qualified Data.Text as T

-- listing 23.1
firstWord :: String
firstWord = "pessimis"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- quick check 23.1
fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- listing 23.2
myWord :: T.Text
myWord = "dog" -- this produces an error without the proper language extension

-- listing 23.3
-- the point here is to show that the literal "dog" does not
-- work with the type Text
-- numeric types don't have this problem
myNum1 :: Int
myNum1 = 3

myNum2 :: Integer
myNum2 = 3

myNum3 :: Double
myNum3 = 3

-- using language extensions we can make the literal "dog" to fit the type Text
-- I added the extension at the top of this file

-- listing 23.4
aWord :: T.Text
aWord = "Cheese"

main :: IO ()
main = do
  print aWord

-- quick check 23.2
-- ghc template.hs -XTemplateHaskell
-- {-# LANGUAGE TemplateHaskell #-}

-- listing 23.5
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- listing 23.6
someText :: T.Text
someText = "Some\ntext for\t you"

-- listing 23.7
breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- quick check 23.3
myLines :: T.Text -> [T.Text]
myLines = T.splitOn "\n"

myUnlines :: [T.Text] -> T.Text
myUnlines = T.intercalate "\n"

