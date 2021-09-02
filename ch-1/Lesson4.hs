import Data.List

isEven :: Integral p => (p -> p) -> p -> p
isEven func x = if even x
    then func x
    else x

names :: [([Char], [Char])]
names = [("Ian", "Curtis"), ("Bernard", "Summer"), ("Peter", "Hook"), ("Stephen", "Morris"), ("Matti", "Summer")]

sorted :: [([Char], [Char])]
sorted = sort names

compareLastNames :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT 
                               else if lastName1 < lastName2
                                   then LT 
                                   else if lastName1 == lastName2
                                       then if firstName1 > firstName2
                                           then GT 
                                           else if firstName1 < firstName2
                                               then LT 
                                               else GT 
                                       else GT 
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

-- listing 4.6
addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = nameText ++ " - " ++ location
    where nameText = fst name ++ " " ++ snd name

-- listing 4.7
sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice :: (a, [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

-- listing 4.8
getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> (\name -> fst name ++ " " ++ snd name)

-- listing 4.9
addressLetterBetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetterBetter name location = locationFunction name
    where locationFunction = getLocationFunction location

-- lesson 4 Q4.1
compareLastNamesNew :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareLastNamesNew name1 name2 = if comparisonResult == EQ 
                               then compare firstName1 firstName2
                               else comparisonResult
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2
          comparisonResult = compare lastName1 lastName2

-- lesson 4 Q4.1
