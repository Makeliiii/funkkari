import qualified Data.Map as Map

-- listing 27.1
successfulRequest :: Maybe Int 
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing 

-- listing 27.2
incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n+1)
incMaybe Nothing  = Nothing

-- quick check 27.1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just str) = (Just . reverse) str
reverseMaybe Nothing    = Nothing

-- listing 27.4
successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

-- quick check 27.2
reversedStr :: Maybe String -> Maybe String
reversedStr str = reverse <$> str

-- listing 27.5
data RobotPart = RobotPart {
    name :: String,
    description :: String,
    cost :: Double,
    count :: Int
} deriving Show

-- listing 27.6
leftArm :: RobotPart
leftArm = RobotPart {
    name = "Left arm",
    description = "left arm for face punching!",
    cost = 1000.00,
    count = 3
}

rightArm :: RobotPart 
rightArm = RobotPart {
    name = "Right arm",
    description = "right arm for kind hand gestures",
    cost = 1025.00,
    count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
    name = "Robot head",
    description = "this head looks mad",
    cost = 5092.25,
    count = 2
}

type Html = String

-- listing 27.7
renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",name part, "</h2>",
                           "<p><h3>desc</h3>", description part,
                           "</p><p><h3>cost</h3>", show $ cost part,
                           "</p><p><h3>count</h3>", show $ count part, "</p>"]

-- listing 27.8
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where
        keys = [1,2,3]
        vals = [leftArm,rightArm,robotHead]
        keyVals = zip keys vals

-- listing 27.9
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- listing 27.10
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- listing 27.11
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

-- listing 27.12
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- listing 27.14
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- listing 27.15
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

-- listing 27.16
htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q27.1
newtype Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box a) = Box (f a)

morePresents :: Box a -> Int -> Box [a] 
morePresents box n = fmap (:boxes box n) box
    where
        boxes (Box a) 0 = []
        boxes (Box a) n = a : boxes (Box a) (n - 1)

-- Q27.3
myBox :: Box Int 
myBox = Box 1

wrap :: Box a -> Box (Box a)
wrap = Box
