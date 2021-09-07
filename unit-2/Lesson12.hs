-- listing 12.1
-- patientInfo :: String -> String -> Int -> Int -> String
-- patientInfo fname lname age height = name ++ " " ++ ageHeight
--  where name = lname ++ ", " ++ fname
--        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "cm.)"

-- listing 12.2
type FirstName = String
type LastName = String
type Age = Int
type Height = Int

-- patientInfo :: FirstName -> LastName -> Age -> Height -> String

-- listing 12.3
type PatientName = (String, String)

-- listing 12.4
firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

-- quick check 12.1
patientInfo :: PatientName -> Age -> Height -> String
patientInfo patient age height = name ++ " " ++ ageHeight
    where name = lastName patient ++ ", " ++ firstName patient
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "cm.)"


-- listing 12.5
data Sex = Male | Female
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

-- listing 12.6
data RhType = Pos | Neg

-- listing 12.7
data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- listing 12.9
-- not taking Rh compatibility into account
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- O is a universal donor
canDonateTo _ (BloodType AB _) = True -- AB is a universal receiver
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False -- otterwise

-- listing 12.10
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

-- listing 12.11
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 :: Name
name1 = Name "Jerome" "Salinger"
name2 :: Name
name2 = NameWithMiddle "Jerome" "David" "Salinger"

-- listing 12.12
data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 175 72 (BloodType AB Pos)

-- quick check 12.2
janeSmith :: Patient
janeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 26 166 56 (BloodType O Neg)

-- listing 12.13
getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

-- listing 12.14
data BetterPatient = BetterPatient { 
    name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
}

jackieSmith :: BetterPatient
jackieSmith = BetterPatient {
    name = Name "Jackie" "Smith",
    age = 43,
    sex = Female,
    height = 165,
    weight = 65,
    bloodType = BloodType O Neg
}

-- quick check 12.3
showings :: String
showings = showName (name jackieSmith)

-- listing 12.15
jackieSmithUpdated :: BetterPatient
jackieSmithUpdated = jackieSmith { age = 44 }

-- Q12.1
canDonateToPatient :: BetterPatient -> BetterPatient -> Bool
canDonateToPatient donator donatee = canDonateTo (bloodType donator) (bloodType donatee)

-- Q12.2
patientSummary :: BetterPatient -> String
patientSummary patient = "******************\n" ++
                         "Patient name: " ++ pName ++ "\n" ++
                         "Sex: " ++ pSex ++ "\n" ++
                         "Age: " ++ pAge ++ "\n" ++
                         "Height: " ++ pHeight ++ "cm" ++ "\n" ++
                         "Weight: " ++ pWeight ++ "kg" ++ "\n" ++
                         "Blood Type: " ++ pBloodType ++ "\n" ++
                         "******************\n"
    where pName = showName (name patient)
          pSex = show (sexInitial (sex patient))
          pAge = show (age patient)
          pHeight = show (height patient)
          pWeight = show (weight patient)
          pBloodType = showBloodType (bloodType patient)
