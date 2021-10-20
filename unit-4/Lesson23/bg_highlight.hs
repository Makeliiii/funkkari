{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- धर्म
--
-- श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।
-- स्वधर्मे निधनं श्रेयः परधर्मो भयावहः

-- listing 23.8
dharma :: T.Text
dharma = "धर्म"

-- listing 23.9
bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

-- listing 23.10
highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

main = do
  TIO.putStrLn (highlight dharma bgText)