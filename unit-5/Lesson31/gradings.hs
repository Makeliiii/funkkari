import qualified Data.Map as Map

-- listing 31.5
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

-- listing 31.6
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

-- listing 31.7
data Candidate = Candidate {
    candidateId :: Int,
    codeReview :: Grade,
    cultureFit :: Grade,
    education :: Degree
}

-- lising 31.8
viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where
        passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding,
                 passedCultureFit,
                 educationMin]

-- listing 31.9
readInt :: IO Int
readInt = getLine >>= return . read

readGrade :: IO Grade
readGrade = getLine >>= return . read

readDegree :: IO Degree
readDegree = getLine >>= return . read

-- listing 31.10
readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id:"
    cId <- readInt
    putStrLn "enter code grade:"
    codeGrade <- readGrade
    putStrLn "enter culture fit grade:"
    cultureGrade <- readGrade
    putStrLn "enter education:"
    degree <- readDegree
    return (Candidate {
        candidateId = cId,
        codeReview  = codeGrade,
        cultureFit  = cultureGrade,
        education   = degree
    })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- listing 31.12
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

-- listing 31.13
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1),
                             (2,candidate2),
                             (3,candidate3)]
                        
-- listing 31.14
assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- quick check 31.4
failPass :: Maybe String -> String
failPass Nothing = "error id not found"
failPass (Just s) = s

-- listing 31.15
candidates :: [Candidate]
candidates = [candidate1,candidate2,candidate3]

-- listing 31.16
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- listing 31.17
assessCandidates :: [Candidate] -> [String]
assessCandidates = map f
    where
        f x = if viable x then "passed" else "failed"

-- listing 31.18
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

