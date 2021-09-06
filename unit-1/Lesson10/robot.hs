robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name,attack,hp) = \message -> message (name,attack,hp)

killerRobot :: (([Char], Integer, Integer) -> t) -> t
killerRobot = robot ("Kill3r", 25, 200)

name :: (a, b, c) -> a
name (n,_,_) = n
attack :: (a, b, c) -> b
attack (_,a,_) = a
hp :: (a, b, c) -> c
hp (_,_,hp) = hp

-- getters
getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot name
getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot attack
getHP :: (((a, b, c) -> c) -> t) -> t
getHP aRobot = aRobot hp

-- setters
setName :: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack :: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP :: (((a, b, c1) -> ((a, b, c2) -> t1) -> t1) -> t2) -> c2 -> t2
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

nicerRobot :: (([Char], Integer, Integer) -> t1) -> t1
nicerRobot = setName killerRobot "kitty"
gentlerRobot :: (([Char], Integer, Integer) -> t1) -> t1
gentlerRobot = setAttack killerRobot 5
softerRobot :: (([Char], Integer, Integer) -> t1) -> t1
softerRobot = setHP killerRobot 50

printRobot :: (Show a1, Show a2) => ((([Char], a1, a2) -> [Char]) -> t) -> t
printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack: " ++ show a ++
                                        " hp: " ++ show h)

damage :: Num c => (((a, b, c) -> ((a, b, c) -> t1) -> t1) -> t2) -> c -> t2
damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))


fight attacker defender = damage defender attack
    where attack = if getHP attacker > 10
                   then getAttack attacker
                   else 0

gentleGiant :: (([Char], Integer, Integer) -> t) -> t
gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 :: (([Char], Integer, Integer) -> t1) -> t1
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 :: (([Char], Integer, Integer) -> t1) -> t1
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 :: (([Char], Integer, Integer) -> t1) -> t1
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 :: (([Char], Integer, Integer) -> t1) -> t1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 :: (([Char], Integer, Integer) -> t1) -> t1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 :: (([Char], Integer, Integer) -> t1) -> t1
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

getHPs :: [((a, b1, c) -> c) -> b2] -> [b2]
getHPs = map getHP
