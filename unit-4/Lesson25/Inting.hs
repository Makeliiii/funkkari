import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- listing 25.1
sampleBytes :: B.ByteString 
sampleBytes = "Hello!"

-- listing 25.2
sampleString :: String
sampleString = B.unpack sampleBytes

-- paskaa
sampleStr :: String
sampleStr = BC.unpack sampleBytes

-- quick check 25.1
bcInt :: BC.ByteString 
bcInt = "6"

toInt :: BC.ByteString -> Int
toInt = read . BC.unpack