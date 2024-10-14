-- 3. Программа работы с файлом предусматривает: просмотр содержимого, добавление новой информации, 
--    удаление какой-либо строки, копирование содержимого в новый файл с использованием двух видов фильтрации ( фильтр выбираем самостоятельно)

import System.IO                    
import Data.Char (toUpper, toLower)
import Control.Exception (evaluate)

readInt :: String -> IO Int
readInt description = do
    putStrLn description
    input <- getLine
    return (read input)

readString :: String -> IO String
readString description = do
    putStrLn description
    input <- getLine
    return input

viewFile :: FilePath -> IO ()       
viewFile filePath = do
    content <- readFile filePath
    putStrLn "Содержимое файла:"
    putStrLn content


addLineToFile :: FilePath -> IO ()
addLineToFile filePath = do
    newLine <- readString "Введите строку для добавления:"
    appendFile filePath ("\n" ++ newLine)         
    putStrLn "Строка добавлена."

deleteLineFromFile :: FilePath -> IO ()
deleteLineFromFile filePath = do
    withFile filePath ReadMode $ \handle -> do
        lineNumber <- readInt "Введите номер строки для удаления:"
        content <- hGetContents handle
        _ <- evaluate (length content)
        let linesContent = lines content
        let newContent = unlines $ deleteElementAt (lineNumber - 1) linesContent

        withFile filePath WriteMode $ \writeHandle -> do
            hPutStr writeHandle newContent   -- hPutStr записывает строку в файл
    putStrLn "Строка удалена."

-- Вспомогательная функция для удаления элемента по индексу
deleteElementAt :: Int -> [a] -> [a]
deleteElementAt idx xs = let (ys, zs) = splitAt idx xs in ys ++ drop 1 zs

copyWithFilter :: FilePath -> FilePath -> IO ()
copyWithFilter inputFile outputFile = do
    content <- readFile inputFile
    filterMode <- readInt "Выберите фильтр: 1 - В верхний регистр, 2 - В нижний регистр"
    let filteredContent = case filterMode of
            1 -> map toUpper content
            2 -> map toLower content
            _   -> content
    writeFile outputFile filteredContent
    putStrLn "Копирование завершено."

main :: IO ()
main = do
    inputFile <- readString "Введите входной файл:"

    mode <- readInt "Выберите действие: 1 - Просмотр, 2 - Добавление, 3 - Удаление, 4 - Копирование с фильтрацией"
    case mode of
        1 -> viewFile inputFile
        2 -> addLineToFile inputFile
        3 -> deleteLineFromFile inputFile
        4 -> do
            outputFile <- readString "Введите имя выходного файла:"
            copyWithFilter inputFile outputFile
        _ -> putStrLn "Нет такого. Выберите из существующих"