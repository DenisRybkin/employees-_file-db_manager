module Main where
import System.IO
    ( hClose,
    hPutStr,
    hPutStrLn,
    openFile,
    Handle,
   IOMode(ReadWriteMode))
import EmployeeData (
     Employee
    ,eAdd                       -- Добавление сотрудника
    ,eEdit                      -- Редактирование сотрудника
    ,eReplaceById               -- Замена сотрудника по его id в списке             
    ,eGetNameSakes              -- Поиск однофамильцев
    ,eSearchByDepartmentName    -- Поиск по подразделению 
    ,eDelete                    -- Удаление сотрудника по id 
    ,eGetBigExperience          -- Поиск сотрудников с предпенсионным возрастом (>= 35)
    ,eGetWithoutEncouragement   -- Поиск сотрудников с без поощрений
    ,eAddEncouragementById      -- Добавление сотруднику по id поощрения
    ,eGetByPosition             -- Поиск сотрудников по заданной должности
    ,eRemoveDuplicates          -- Удаления дублирующихся пользователей (с одинаковым id)
    ,eGetAvgExperience          -- Получениие среднего стажа сотрудников на определенной позиции
    ,eGetSortByExperience       -- Сортировка сотрудников по стажу
    ,deserializeEs,             -- Десериализация сотрудников
    getLastName
    )            
import Control.Monad ()
import qualified Utils
import Control.Exception (handle)
import Distribution.Simple.PackageIndex (searchByName)

type Writter = Handle->[Employee]->IO()

type ControllerIO = Handle-> [Employee] -> IO()
type Controller = [Employee] -> IO()

printEmployees::[Employee] -> IO()
printEmployees [] = putStrLn "-------------"
printEmployees (x:xs) = do
  print x
  printEmployees xs

writeEmployees'::Writter
writeEmployees' handle [] = hPutStr handle ""
writeEmployees' handle (x:xs) = do
  let employee = show x
  hPutStrLn handle employee
  writeEmployees' handle xs

writeEmployees::Writter
writeEmployees oldHandle employees = do
  hClose oldHandle
  writeFile "data.txt" ""
  handle <- openFile "data.txt" ReadWriteMode
  writeEmployees' handle employees
  hClose handle

readEmployees::Handle->IO [String]
readEmployees handle = do
  lines <- Utils.readFilledLines handle
  return lines  

addEmployee::ControllerIO
addEmployee handle employees = do
  putStrLn "Enter: name, id, department, position, experience, list encouragement"
  (name, id, department, position, experience, listEncouragement) <- Utils.readEmployee
  let employee = eAdd (name,id) (department, position) experience listEncouragement 
  let newList = employees ++ [employee]
  writeEmployees handle newList 

editEmployee::ControllerIO 
editEmployee handle employees = do
    putStrLn "Enter: name, id, department, position, experience, list encouragement"
    (name, id, department, position, experience, listEncouragement) <- Utils.readEmployee
    let newEmployee = eAdd (name,id) (department, position) experience listEncouragement
    let newList = eReplaceById employees id newEmployee
    writeEmployees handle newList

deleteEmployee::ControllerIO
deleteEmployee handle employees = do 
    putStrLn "Enter: id of the employee"
    strId <- getLine
    let id = read strId :: Int
    let newList = eDelete employees id
    print newList
    writeEmployees handle newList

searchByBigExperience::Controller
--searchByBigExperience _ [] = printEmployees []
searchByBigExperience employees = 
    printEmployees (eGetBigExperience employees)

searchByDepartmentName::ControllerIO
--searchByDepartmentName _ [] = printEmployees []
searchByDepartmentName handle employees = do
  putStrLn "Enter the department mame"
  department <- getLine
  printEmployees (eSearchByDepartmentName employees department)

searchNamesakes::Controller
--searchNamesakes _ [] = printEmployees []
searchNamesakes employees = printEmployees (eGetNameSakes employees)

sortByExperience::Controller
sortByExperience employees = printEmployees (eGetSortByExperience employees)

deleteDuplicates::ControllerIO
deleteDuplicates handle employees = writeEmployees handle( eRemoveDuplicates employees)

searchAvgExperience::ControllerIO
searchAvgExperience handle employees = do
    putStrLn "Enter the target position"
    targetPosition <- getLine
    print (eGetAvgExperience employees targetPosition) 

searchWithoutEncouragement::Controller
searchWithoutEncouragement employees = printEmployees (eGetWithoutEncouragement employees)

addEncouragementById::ControllerIO
addEncouragementById handle employees = do
    putStrLn "Enter target id of the user"
    eId <- getLine
    let id = read eId :: Int
    putStrLn "Enter a Encouragements"
    encouragements <- Utils.readEncouragements
    writeEmployees handle (eAddEncouragementById employees id encouragements)

searchByPosition::ControllerIO
searchByPosition handle employees = do
    putStrLn "Enter target position"
    position <- getLine
    printEmployees (eGetByPosition employees position)
 
userCommandHandler::Handle -> [Employee] -> String -> IO()
userCommandHandler handle dEmployees command
  | command == "1" = printEmployees dEmployees
  | command == "2" = addEmployee handle dEmployees 
  | command == "3" = editEmployee handle dEmployees
  | command == "4" = searchByBigExperience dEmployees
  | command == "5" = searchByDepartmentName handle dEmployees
  | command == "6" = deleteEmployee handle dEmployees
  | command == "7" = searchNamesakes dEmployees
  | command == "8" = sortByExperience dEmployees
  | command == "9" = deleteDuplicates handle dEmployees
  | command == "10" = searchAvgExperience handle dEmployees
  | command == "11" = searchWithoutEncouragement dEmployees
  | command == "12" = addEncouragementById handle dEmployees
  | command == "13" = searchByPosition handle dEmployees
  | otherwise = putStrLn "Unknow comand"


setup::Handle->IO()
setup handle = do
  employees <- readEmployees handle
  let dEmployees = deserializeEs employees
  print $ map getLastName dEmployees
  user_command <- Utils.setupMenu
  userCommandHandler handle dEmployees user_command

main::IO()
main = do
    handle <- openFile "data.txt" ReadWriteMode
    setup handle
    hClose handle
    putStrLn "End"