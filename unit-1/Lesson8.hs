-- consider this
-- this took way longer than it should've
myDrop :: Int -> [Int] -> [Int]
myDrop n list = if n > length list
                then error "n larger than list length"
                else edit n list []
    where listLength = length list
          edit n currList list = if n == length currList
                                 then list
                                 else edit (n + 1) currList (list ++ [n + 1])