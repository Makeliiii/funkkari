-- listing 22.1
import System.Environment ( getArgs )
import Control.Monad

-- listing 22.2 - 22.3 - 22.4 - 22.5 - 22.6
main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)

-- quick check 22.1
exampleMain :: IO ()
exampleMain = do
    vals <- mapM (const getLine) [1..3]
    mapM_ putStrLn vals

-- quick check 22.2
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n func = mapM (const func) [1..n]