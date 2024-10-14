-- 3. Программа работы с файлом предусматривает: просмотр содержимого, добавление новой информации, 
--    удаление какой-либо строки, копирование содержимого в новый файл с использованием двух видов фильтрации ( фильтр выбираем самостоятельно)

import System.IO                     --предоставляет функции для работы с файлами и вводом/выводом в Haskell
import Data.Char (toUpper, toLower)
import Control.Exception (evaluate)

-- Получение целого числа от пользователя
-- Принимает строку, которая будет выведена как подсказка и возвращает введенное число
readInt :: String -> IO Int
readInt description = do
    putStrLn description
    input <- getLine
    return (read input)

-- Получение строки от пользователя
-- Принимает строку, которая будет выведена как подсказка и возвращает введенную строка
readString :: String -> IO String
readString description = do
    putStrLn description
    input <- getLine
    return input


-- Функция для отображения содержимого файла
viewFile :: FilePath -> IO ()         --IO () — функция выполняет побочные эффекты (работа с файлом)
viewFile filePath = do
    content <- readFile filePath
    putStrLn "Содержимое файла:"
    putStrLn content


-- Функция для добавления новой строки в файл
addLineToFile :: FilePath -> IO ()
addLineToFile filePath = do
    newLine <- readString "Введите строку для добавления:"
    appendFile filePath ("\n" ++ newLine)               -- это функция, которая добавляет данные в конец файла, не удаляя его текущее содержимое
    putStrLn "Строка добавлена."

deleteLineFromFile :: FilePath -> IO ()
deleteLineFromFile filePath = do
    -- Открываем файл для чтения
    -- withFile — это функция для безопасной работы с файлами. Она автоматически открывает файл и закрывает его после завершения работы, handle - указатель на открытый файл (дескриптор)
    withFile filePath ReadMode $ \handle -> do
        -- Пользователь вводит строку, и она сохраняется в переменную newLine
        lineNumber <- readInt "Введите номер строки для удаления:"

        content <- hGetContents handle  -- hGetContents - считывает все содержимое файла в строку, используя дескриптор handle
        _ <- evaluate (length content)  -- принудительно оценить ленивое вычисление (по мере необходимости) содержимого файла, прочитанного через hGetContents 
        -- файл открыт для чтения, поэтому не может открыться для записи
        let linesContent = lines content -- lines разделяет на строки

        -- unlines — превращает список строк обратно в одну строку с символами новой строки
        -- $ оператор, после него идут опреаторы   чтобы не писать не писать скобки
        let newContent = unlines $ deleteElementAt (lineNumber - 1) linesContent

        -- Открываем файл для записи
        withFile filePath WriteMode $ \writeHandle -> do
            hPutStr writeHandle newContent   -- hPutStr записывает строку в файл
    putStrLn "Строка удалена."

-- Вспомогательная функция для удаления элемента по индексу
deleteElementAt :: Int -> [a] -> [a]
--splitAt idx xs — это функция, которая разбивает список xs на две части (...)(idx...)  (in ... — это часть, где мы можем использовать переменные, определенные в let.)
deleteElementAt idx xs = let (ys, zs) = splitAt idx xs in ys ++ drop 1 zs

-- Функция для копирования с фильтрацией
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