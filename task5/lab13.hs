-- 3. Программа работы с файлом предусматривает: просмотр содержимого, добавление новой информации, 
--    удаление какой-либо строки, копирование содержимого в новый файл с использованием двух видов фильтрации ( фильтр выбираем самостоятельно)

import System.IO
import System.Environment (getArgs)
import Data.Char (toUpper, toLower)
import Control.Exception (evaluate)

viewFile :: FilePath -> IO ()
viewFile filePath = do
    content <- readFile filePath
    putStrLn "Содержимое файла:"
    putStrLn content

addLineToFile :: FilePath -> String -> IO ()
addLineToFile filePath newLine = do
    appendFile filePath ("\n" ++ newLine)
    putStrLn "Строка добавлена."


deleteLineFromFile :: FilePath -> Int -> IO ()
deleteLineFromFile filePath lineNumber = do
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        _ <- evaluate (length content)
        let linesContent = lines content
        let newContent = unlines $ deleteElementAt (lineNumber - 1) linesContent

        withFile filePath WriteMode $ \writeHandle -> do
            hPutStr writeHandle newContent
    putStrLn "Строка удалена."

deleteElementAt :: Int -> [a] -> [a]
deleteElementAt idx xs = let (ys, zs) = splitAt idx xs in ys ++ drop 1 zs

copyWithFilter :: FilePath -> FilePath -> Int -> IO ()
copyWithFilter inputFile outputFile filterMode = do
    content <- readFile inputFile
    let filteredContent = case filterMode of
            1 -> map toUpper content
            2 -> map toLower content
            _ -> content
    writeFile outputFile filteredContent
    putStrLn "Копирование завершено."


main :: IO ()
main = do
    args <- getArgs
    case args of
        ("view" : inputFile : _) -> viewFile inputFile

        ("add" : inputFile : newLine : _) -> addLineToFile inputFile newLine

        ("delete" : inputFile : lineStr : _) -> do
            let lineNumber = read lineStr
            deleteLineFromFile inputFile lineNumber

        ("copy" : inputFile : outputFile : filterModeStr : _) -> do
            let filterMode = read filterModeStr
            copyWithFilter inputFile outputFile filterMode

        _ -> putStrLn "Usage: view <file> | add <file> <line> | delete <file> <line_number> | copy <file> <output_file> <filter_mode>"