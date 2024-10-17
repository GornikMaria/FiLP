-- 3. Программа работы с файлом предусматривает: просмотр содержимого, добавление новой информации, 
--    удаление какой-либо строки, копирование содержимого в новый файл с использованием двух видов фильтрации ( фильтр выбираем самостоятельно)

import System.IO
import System.Environment
import Data.List
import System.Environment (getArgs)

viewFile :: FilePath -> IO ()
viewFile filename = do
    contents <- readFile filename
    putStrLn contents

addToFile :: FilePath -> String -> IO ()
addToFile filename content = do
    appendFile filename (content ++ "\n")
    putStrLn "Content added successfully."

deleteLine :: FilePath -> Int -> IO ()
deleteLine filename lineNumber = do
    contents <- readFile filename
    let linesOfFile = lines contents
    let newContents = unlines $ deleteAt (lineNumber - 1) linesOfFile
    writeFile filename newContents
    putStrLn "Line deleted successfully."

copyFileWithFilter :: FilePath -> FilePath -> String -> IO ()
copyFileWithFilter source target filterType = do
    contents <- readFile source
    let filteredContents = filterContent (lines contents) filterType
    writeFile target (unlines filteredContents)
    putStrLn "File copied with filter successfully."

filterContent :: [String] -> String -> [String]
filterContent lines filterType
    | filterType == "length_greater_than_10" = filter (\line -> length line > 10) lines
    | filterType == "contains_hello" = filter (isInfixOf "hello") lines
    | otherwise = lines  -- если фильтр неизвестный, просто вернуть все строки

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs ++ drop (idx + 1) xs


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["view", filename] -> viewFile filename
        ["add", filename, content] -> addToFile filename content
        ["delete", filename, lineNumberStr] -> do
            let lineNumber = read lineNumberStr :: Int
            deleteLine filename lineNumber
        ["copy", source, target, filterType] -> do
            copyFileWithFilter source target filterType
        _ -> putStrLn "Usage: view|add|delete|copy [filename] [content|lineNumber|target] [filterType]"
