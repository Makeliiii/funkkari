import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- listing 25.4
intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where
        safeInt = int `mod` 255

-- listing 25.5
intToBC :: Int -> BC.ByteString 
intToBC int = BC.pack [intToChar int]

-- listing 25.6
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString 
replaceByte  loc charVal bytes = mconcat [before,newChar,after]
    where
        (before,rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

-- listing 25.7
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLenght = BC.length bytes
    location <- randomRIO (1,bytesLenght)
    charVal <- randomRIO (0,255)
    return (replaceByte location charVal bytes)

-- quick check 25.3
randomChar :: IO Char
randomChar = do
    randomInt <- randomRIO (0,255)
    return (toEnum randomInt)

-- listing 25.8
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
    where
        (before,rest) = BC.splitAt start bytes
        (target,after) = BC.splitAt size rest
        changed = BC.reverse (BC.sort target)

-- listing 25.9
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLenght = BC.length bytes
    start <- randomRIO (0,bytesLenght - sectionSize)
    return (sortSection start sectionSize bytes)

-- listing 25.3
main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    imageFile <- BC.readFile filename
    glitched <- randomSortSection imageFile
    let glitchedFileName = mconcat ["glitched_", filename]
    BC.writeFile glitchedFileName glitched
    print "all done"