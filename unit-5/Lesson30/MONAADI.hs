import qualified Data.Map as Map

-- listing 30.1
main :: IO ()
main = do
    putStrLn "Remember do notation!"
    putStrLn "It makes things easy"

-- listing 30.2
type Username = String
type GamerId = Int
type PlayerCredits = Int

usernameDB :: Map.Map GamerId Username
usernameDB = Map.fromList [(1, "nYarlathoTep"),
                           (2, "KINGinYELLOW"),
                           (3, "dagon1997"),
                           (4, "rcarter1919"),
                           (5, "xCTHULHUx"),
                           (6, "yogSOThoth")]

creditsDB :: Map.Map Username PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep", 2000),
                           ("KINGinYELLOW", 15000),
                           ("dagon1997", 300),
                           ("rcarter1919", 12),
                           ("xCTHULHUx", 50000),
                           ("yogSOThoth", 150000)]

-- listing 30.4
lookupUsername :: GamerId -> Maybe Username
lookupUsername id = Map.lookup id usernameDB

lookupCredits :: Username -> Maybe PlayerCredits
lookupCredits name = Map.lookup name creditsDB

-- llisting 30.5
altLookupCredits :: Maybe Username -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

-- listing 30.6
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId = altLookupCredits . lookupUsername

-- listing 30.7
creditsFromID :: GamerId -> Maybe PlayerCredits
creditsFromID id = lookupUsername id >>= lookupCredits

-- listing 30.8
type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1),
                          (1002,2),
                          (1003,3),
                          (1004,4),
                          (1005,5),
                          (1006,6)]
                        
lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

-- lsiting 30.9
creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUsername >>= lookupCredits

