module EmployeeData 
    (
        Employee (..),              -- Объект сотрудника
        eAdd,                       -- Добавление сотрудника
        eEdit,                      -- Редактирование сотрудника
        eReplaceById,               -- Замена сотрудника по его id в списке 
        eGetNameSakes,              -- Поиск однофамильцев
        eSearchByDepartmentName,    -- Поиск по подразделению 
        eDelete,                    -- Удаление сотрудника по id 
        eGetBigExperience,          -- Поиск сотрудников с предпенсионным возрастом (>= 35)
        eGetWithoutEncouragement,   -- Поиск сотрудников с без поощрений
        eAddEncouragementById,      -- Добавление сотруднику по id поощрения
        eGetByPosition,             -- Поиск сотрудников по заданной должности
        eRemoveDuplicates,          -- Удаления дублирующихся пользователей (с одинаковым id)
        eGetAvgExperience,          -- Получениие среднего стажа сотрудников на определенной позиции
        eGetSortByExperience,       -- Сортировка сотрудников по стажу
        deserializeEs               -- Десериализация сотрудников
        ,getLastName
    )
where
import System.Win32.Automation.Input (xBUTTON2)

data Employee = Employee 
    { name_id :: (String, Int)
    , departmentName_position :: (String, String)
    , experience :: Int
    , listEncouragement:: [String]
    } deriving (Eq,Show,Read,Ord)

deserializeEs'::[String]->[Employee]->[Employee]
deserializeEs' [] acc = acc
deserializeEs' (x:xs) acc = deserializeEs' xs (acc ++ [read x :: Employee])

deserializeEs::[String]->[Employee]
deserializeEs list = deserializeEs' list []    

filter':: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' fn list = [ x | x <- list, not (fn x)]

find':: (a -> Bool) -> [a] -> [a]
find' _ [] = []
find' fn (x:xs) = if fn x then x:find' fn xs else find' fn xs


getFirst:: (a,a) -> a
getFirst(x, y) = x;

getSecond:: (a,a) -> a
getSecond(x, y) = y;

getDepartment:: Employee -> String
getDepartment (Employee _ x _ _) = getFirst x

getId:: Employee -> Int
getId (Employee (x, y) _ _ _) = y

getNameId:: Employee -> (String, Int)
getNameId (Employee x _ _ _) = x

getPosition:: Employee -> String
getPosition (Employee _ (x,y) _ _) = y

getDepartmentNamePosition:: Employee -> (String, String)
getDepartmentNamePosition (Employee _ x _ _) = x

getExperience:: Employee -> Int
getExperience (Employee _ _ x _) = x

getEncouragement:: Employee -> [String]
getEncouragement (Employee _ _ _ x) = x

getLenght :: [a] -> Int
getLenght [] = 0
getLenght list = foldr (\ x -> (+) 1) 0 list

getSumList:: [Int] -> Int
getSumList list = foldr (+) 0 list

getNameTail:: String -> String
getNameTail " " = []
getNameTail "" = []
getNameTail (x:xs) = if (x == ' ' && incudes xs ' ' == False) then xs else getNameTail xs

getFirstWord:: String -> String
getFirstWord " " = []
getFirstWord "" = []
getFirstWord (x:xs) = if x == ' ' then [] else x : getFirstWord xs

getLastName:: Employee -> String
getLastName (Employee (name, id) _ _ _) = getFirstWord name

compareEById:: Employee -> Int -> Bool
compareEById e id = (getId e /= id) || True

incudes:: Eq a => [a] -> a -> Bool
incudes [] _ = False
incudes (x:xs) target = (x == target) || incudes xs target

eShapeGetNameSakes :: [Employee] -> [Employee] -> [Employee]
eShapeGetNameSakes [] _ = []
eShapeGetNameSakes (x:xs) acc = 
    if 
        incudes (map getLastName xs) currentName 
        || 
        incudes (map getLastName acc) currentName
    then x:eShapeGetNameSakes xs (x : acc)
    else  eShapeGetNameSakes xs acc 
    where
        currentName = getLastName x

eGetNameSakes :: [Employee] -> [Employee]
eGetNameSakes [] = []
eGetNameSakes employees = eShapeGetNameSakes employees []


eAdd :: (String, Int) -> (String, String) -> Int -> [String] -> Employee
eAdd a b c d = Employee {
    name_id = a
    ,departmentName_position = b
    ,experience = c
    ,listEncouragement = d
 }

eEdit:: Employee -> (String, Int) -> (String, String) -> Int -> [String] -> Employee
eEdit employee a b c d = employee { 
    name_id = a
    ,departmentName_position = b
    ,experience = c
    ,listEncouragement = d
    }

eReplaceById:: [Employee] -> Int -> Employee -> [Employee]
eReplaceById [] _ _ = []
eReplaceById (x:xs) id e = if compareEById e id 
        then e:eReplaceById xs id e
        else x:eReplaceById xs id e

eSearchByDepartmentName:: [Employee] -> String -> [Employee]
eSearchByDepartmentName list filterParam = 
    find' (\x -> getDepartment x  == filterParam) list

eDelete:: [Employee] -> Int -> [Employee]
eDelete [] _ = []
eDelete list id = filter (\ x -> getId x  /= id) list

eGetBigExperience:: [Employee] -> [Employee]
eGetBigExperience [] = []
eGetBigExperience list = find' (\x -> getExperience x >= 35) list 

eGetWithoutEncouragement:: [Employee] -> [Employee]
eGetWithoutEncouragement [] = []
eGetWithoutEncouragement list = find' (\x -> getLenght (getEncouragement x) == 0) list

eAddEncouragementById:: [Employee] -> Int -> [String] -> [Employee]
eAddEncouragementById [] _ _ = []
eAddEncouragementById (x:xs) id encouragements = if getId x == id
    then Employee {
        name_id = getNameId x,
        departmentName_position = getDepartmentNamePosition x,
        experience = getExperience x,
        listEncouragement = getEncouragement x ++ encouragements
        } : eAddEncouragementById xs id encouragements
    else x :eAddEncouragementById xs id encouragements 

eGetByPosition:: [Employee] -> String -> [Employee]
eGetByPosition [] _ = []
eGetByPosition list filter = find' (\x -> getPosition x == filter) list

eRemoveDuplicates:: [Employee] -> [Employee]
eRemoveDuplicates [] = []
eRemoveDuplicates (x:xs) = if incudes convertXS (getId x)
    then eRemoveDuplicates xs 
    else x: eRemoveDuplicates xs
    where convertXS = map getId xs

eGetAvgExperience:: [Employee] -> String -> Double
eGetAvgExperience [] _ = 0
eGetAvgExperience list position = 
    fromIntegral (getSumList (map getExperience (eGetByPosition list position)))
    / 
    fromIntegral (getLenght (eGetByPosition list position))  

eGetSortByExperience:: [Employee] -> [Employee] 
eGetSortByExperience [] = []
eGetSortByExperience (x:xs) = 
    eGetSortByExperience [y|y <- xs, getExperience y < getExperience x]
     ++ [x]
      ++ eGetSortByExperience [y|y <- xs, getExperience y>= getExperience x]