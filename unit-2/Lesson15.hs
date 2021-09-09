-- listing 15.1
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

-- listing 15.2
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

-- listing 15.3
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

-- listing 15.4
rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
    where sizeOfAlphabet = 1 + largestCharNumber

-- listing 15.5
message :: [FourLetterAlphabet]
message = [L1,L2,L1,L4,L3,L1,L4,L3]

-- listing 15.6
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map (rotN 4)

-- listing 15.7
data ThreeLetterAlphabet = Alpha | Beta | Omega deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Omega]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map (rotN 3)

-- listing 15.8
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation = offset `mod` n

-- listing 15.9
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map (rotNdecoder 3)

-- listing 15.10
rotEncoder :: String -> String
rotEncoder = map rotChar
    where size = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN size

rotDecoder :: String -> String
rotDecoder = map rotCharDe
    where size = 1 + fromEnum (maxBound :: Char)
          rotCharDe = rotNdecoder size

-- 15.2 cryptography shit
-- listing 15.11
xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && not (x && y)

-- listing 15.12
xorPair :: (Bool, Bool) -> Bool
xorPair (x, y) = xorBool x y

-- listing 15.13
xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair (zip xs ys)

-- listing 15.14
type Bits = [Bool]

-- listing 15.15
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where remainder = n `mod` 2
          nextVal = n `div` 2

-- listing 15.16
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - length reversedBits
          leadingFalses = take missingBits (cycle [False])

-- listing 15.17
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

-- listing 15.18
bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^snd x) trueLocations)
    where size = length bits
          indices = [size - 1, size - 2 .. 0]
          trueLocations = filter fst (zip bits indices)

-- listing 15.19
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- own shit
-- easy life :sadge:
encodeString :: String -> [Bits]
encodeString = map charToBits

decodeBits :: [Bits] -> String
decodeBits = map bitsToChar

-- 15.4 one time pad thing
-- listing 15.20
myPad :: String
myPad = "Shhhhhh"

-- listing 15.21
myPlainText :: String 
myPlainText = "Haskell"

-- listing 15.22
applyOTP' :: String -> String -> [Bits]
applyOTP' pad string = map (\pair -> fst pair `xor` snd pair) (zip padBits stringBits)
    where padBits = encodeString pad
          stringBits = encodeString string

-- listing 15.23
applyOTP :: String -> String -> String
applyOTP pad str = decodeBits (applyOTP' pad str)

-- listing 15.24
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

-- listing 15.25
class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

-- listing 15.26
data Rot = Rot

-- listing 15.27
instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

-- listing 15.28
data OneTimePad = OTP String

-- listing 15.29
instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

-- listing 15.30
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- listing 15.31
prng :: Int -> Int -> Int -> Int -> Int 
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

-- listing 15.32
examplePRNG :: Int -> Int 
examplePRNG = prng 1337 7 100

-- own shit
data StreamCipher = StreamCipher Int

genPad :: Int -> [Int] -> [Int]
genPad x xs = x:genPad (examplePRNG x) xs

instance Cipher StreamCipher where
    encode (StreamCipher x) text = applyOTP pad text
        where size = length text
              pad  = map toEnum (genPad x [])
    decode (StreamCipher x) text = applyOTP pad text
        where size = length text
              pad  = map toEnum (genPad x [])