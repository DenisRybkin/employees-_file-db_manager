module Utils (split, filter, readFilledLines, setupMenu, readEncouragements, readEmployee) where
import System.IO
import EmployeeData (Employee)

type Compare a = a->a->Bool 

-- methods
tupleConcat::([a], a) -> [a]
tupleConcat (list, new) = list ++ [new]

-- split string to words
split'::String->String->[String]->[String]
split' [] word list = list ++ [word]
split' (x:xs) word list = 
  if x /= ' ' then split' xs (word ++ [x]) list 
  else split' xs "" (list ++ [word])

split::String->[String]
split str = split' str "" []

-- read lines of file
readLines'::Handle->[String]->IO [String]
readLines' h acc = do
  isFileEnd <- hIsEOF h
  if isFileEnd
    then return acc
    else do
      line <- hGetLine h
      readLines' h (acc ++ [line])

readLines::Handle->IO [String]
readLines h = readLines' h []

-- read only filled (not "") lines of file
readFilledLines::Handle->IO [String]
readFilledLines h = do
  lines <- readLines h
  return (filter (not . null) lines)

-- setup menu
setupMenu::IO String
setupMenu = do
  putStrLn "Menu:"
  putStrLn "1. Print All\n2. Add employee\n3. Edit employee\n4. Print Olds"
  putStrLn "5. Print & searching by departament name\n6. Delete employee\n7. Print of namesakes\n8. Print sorted by experience"
  putStrLn "9. Delete duplicates\n10. Print average experience\n11. Print employees without encouragements"
  putStrLn "12. Add encouragements to employee\n13. Print employees by position"
  command <- getLine
  return (command)

readEncouragements::IO [String]
readEncouragements = do
  rwd <- getLine
  if rwd /= "" 
    then return (filter (not . null) (Utils.split rwd)) 
    else return ([])

readEmployee::IO (String, Int, String, String, Int, [String])
readEmployee = do
  name <- getLine
  num <- getLine
  department <- getLine
  jobTitle <- getLine
  exp <- getLine
  rewards <- readEncouragements

  let number = read num :: Int
  let experience = read exp :: Int

  return (name, number, department, jobTitle, experience, rewards)
