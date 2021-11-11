import Control.Monad (guard)
import Control.Applicative (Alternative)
-- listing 33.1
data Name = Name {
    firstName :: String,
    lastName  :: String
}

instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

-- listing 33.2
data GradeLevel = Freshman | Sophmore | Junior | Senior deriving (Eq, Ord, Enum, Show)

-- listing 33.3
data Student = Student {
    studentId   :: Int,
    gradeLevel  :: GradeLevel,
    studentName :: Name
} deriving Show

-- listing 33.4
students :: [Student]
students = [
    Student 1 Senior (Name "Audre" "Lorde"),
    Student 2 Junior (Name "Leslie" "Silko"),
    Student 3 Freshman (Name "Judith" "Butler"),
    Student 4 Senior (Name "Guy" "Debord"),
    Student 5 Sophmore (Name "Jean" "Baudrillard"),
    Student 6 Junior (Name "Julia" "Kristeva")
    ]

-- listing 33.5
_select prop vals = do
    val <- vals
    return (prop val)

-- listing 33.6
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- listing 33.7
startsWith :: Char -> String -> Bool
startsWith char string = char == head string

-- listing 33.8
data Teacher = Teacher {
    teacherId :: Int,
    teacherName :: Name
} deriving Show

-- listing 33.9
teachers :: [Teacher]
teachers = [
    Teacher 100 (Name "Simone" "De Beauvior"),
    Teacher 200 (Name "Susan" "Sontag")
    ]

-- listing 33.10
data Course = Course {
    courseId :: Int,
    courseTitle :: String,
    teacher :: Int
} deriving Show

-- listing 33.11
courses :: [Course]
courses = [
    Course 101 "French" 100,
    Course 201 "English" 200
    ]

_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let pairs = (d1, d2)
    guard (prop1 (fst pairs) == prop2 (snd pairs))
    return pairs

-- listing 33.12
joinData :: [(Teacher, Course)]
joinData = _join teachers courses teacherId teacher
whereResult :: [(Teacher, Course)]
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult :: [Name]
selectResult = _select (teacherName . fst) whereResult

-- listing 33.13
_hinq selectQuery joinQuery whereQuery = (\joinData -> (\whereResult -> selectQuery whereResult) (whereQuery joinData)) joinQuery

-- listing 33.14
finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

-- listing 33.15
teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (const True))

-- listing 33.16
_select :: Monad m => (a -> b) -> m a -> m b
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) | HINQ_ (m a -> m b) (m a)

-- listing 33.17
runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (const True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers
    
-- listing 33.18
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

-- listing 33.19
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

--listing 33.20
missingCourse :: Maybe Course
missingCourse = Nothing 

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

-- listing 33.21
data Enrollment = Enrollment {
    student :: Int,
    course  :: Int
} deriving Show

-- listing 33.22
enrollments :: [Enrollment]
enrollments = [
    Enrollment 1 101,
    Enrollment 2 101,
    Enrollment 2 201,
    Enrollment 3 101,
    Enrollment 4 201,
    Enrollment 4 101,
    Enrollment 5 101,
    Enrollment 6 201
    ]

-- listing 33.23
studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ = HINQ_ (_select (\(st,en) -> (studentName st, course en))) (_join students enrollments studentId student)

-- listing 33.24
studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

-- listing 33.25
englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ = HINQ (_select (fst . fst)) (_join studentEnrollments courses snd courseId) (_where ((== "English") . courseTitle . snd))

-- listing 33.26
englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

-- listing 33.27
getEnrollments :: String -> [Name]
getEnrollments str = runHINQ courseQuery
    where
        courseQuery = HINQ (_select (fst . fst)) (_join studentEnrollments courses snd courseId) (_where ((== str) . courseTitle . snd))

