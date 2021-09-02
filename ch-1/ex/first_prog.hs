messyMain :: IO()
messyMain = do
    print "Who is the email for?"
    recipient <- getLine 
    print "What is the title?"
    title <- getLine 
    print "Who is the author?"
    author <- getLine 
    print ("Dear " ++ recipient ++ ",\n" ++
           "Thanks for buying " ++ title ++ "\nthanks,\n" ++
           author)

toPart :: [Char] -> [Char]
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: [Char] -> [Char]
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ "\n"

fromPart :: [Char] -> [Char]
fromPart author = "Thanks,\n" ++ author

createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

main :: IO ()
main = do
    print "Who is the email for?"
    recipient <- getLine 
    print "What is the title?"
    title <- getLine 
    print "Who is the author?"
    author <- getLine 
    print (createEmail recipient title author)