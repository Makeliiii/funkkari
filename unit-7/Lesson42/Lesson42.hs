import Data.Array.Base ((//),(!), array, UArray, listArray, IArray (bounds))
import Data.Array.ST
    ( readArray,
      thaw,
      writeArray,
      MArray(newArray),
      STUArray,
      runSTUArray )
import Control.Monad ( when, forM_ )
import Control.Monad.ST ( ST )

-- listing 42.1
aLargeList :: [Int]
aLargeList = [1 .. 1000000]

-- listing 42.2
aLargeArray :: UArray Int Int
aLargeArray = array (0, 999999) []

-- listing 42.3
aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList

-- listing 42.4
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

-- lsiting 42.5
oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1..10] $ repeat True

-- listing 42.6
beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []

-- listing 42.7
updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)]

-- listing 42.8
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

-- listing 32.10
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

-- listing 42.12
myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

-- listing 42.13
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1..end] $ \i -> do
        forM_ [0..(end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j+1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j+1) val
    return stArray

