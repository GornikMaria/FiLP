import System.Environment (getArgs)
import Data.Char (isPunctuation)
import System.IO

main = do
    params <- getArgs
    case params of
        [inputFile, outputFile] -> do
            putStrLn "Введите символ для замены знаков пунктуации:"
            replacement <- getChar
            text <- readFile inputFile
            let modifiedText = map (\ch -> if isPunctuation ch then replacement else ch) text
            writeFile outputFile modifiedText
            putStrLn $ "Файл '" ++ inputFile ++ "' был обработан и сохранен как '" ++ outputFile ++ "' с заменой знаков пунктуации на '" ++ [replacement] ++ "'."
        _ -> putStrLn "Ошибка: Укажите два аргумента - исходный файл и файл для записи."
